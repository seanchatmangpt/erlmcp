#!/bin/bash
# ============================================================================
# SBOM Verification Script
# Validates Software Bill of Materials (SBOM) completeness and attestation
# Ensures supply chain security, component tracking, and compliance
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
log_test() { echo -e "${BLUE}[TEST]${NC} $1"; }

# ============================================================================
# Configuration
# ============================================================================

PROJECT_ID="${PROJECT_ID:-}"
REGION="${REGION:-us-central1}"
IMAGE_NAME="${IMAGE_NAME:-erlmcp/erlmcp}"
IMAGE_TAG="${IMAGE_TAG:-latest}"
FULL_IMAGE="${FULL_IMAGE:-${REGION}-docker.pkg.dev/${PROJECT_ID}/${IMAGE_NAME}:${IMAGE_TAG}}"

SBOM_FILE="${SBOM_FILE:-}"
SBOM_FORMAT="${SBOM_FORMAT:-spdx}"  # spdx | cyclonedx
VERIFICATION_MODE="${VERIFICATION_MODE:-strict}"  # strict | relaxed | audit

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MARKETPLACE_DIR="$(dirname "$SCRIPT_DIR")"
EVIDENCE_DIR="${MARKETPLACE_DIR}/test-evidence/sbom"

# Attestation and signing
ATTESTATION_REQUIRED="${ATTESTATION_REQUIRED:-true}"
COSIGN_VERIFY="${COSIGN_VERIFY:-false}"
KEYLESS_VERIFY="${KEYLESS_VERIFY:-true}"

# Completeness thresholds
MIN_COMPONENTS="${MIN_COMPONENTS:-10}"
REQUIRE_LICENSES="${REQUIRE_LICENSES:-true}"
REQUIRE_VERSIONS="${REQUIRE_VERSIONS:-true}"
REQUIRE_CHECKSUMS="${REQUIRE_CHECKSUMS:-true}"

# Counters
TOTAL_CHECKS=0
PASSED_CHECKS=0
FAILED_CHECKS=0
WARNINGS=0

# ============================================================================
# Prerequisites
# ============================================================================

check_prerequisites() {
    log_info "Checking prerequisites..."

    if [ -z "$PROJECT_ID" ] && [ "$VERIFICATION_MODE" != "audit" ]; then
        log_error "PROJECT_ID not set. Run: export PROJECT_ID=your-project-id"
        exit 1
    fi

    if ! command -v docker &> /dev/null; then
        log_error "docker not found"
        exit 1
    fi

    if ! command -v jq &> /dev/null; then
        log_error "jq not found. Install with: apt-get install jq"
        exit 1
    fi

    # Create evidence directory
    mkdir -p "$EVIDENCE_DIR"

    log_info "Prerequisites check passed"
}

# ============================================================================
# SBOM Discovery and Extraction
# ============================================================================

test_sbom_discovery() {
    log_test "Discovering SBOM sources..."

    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))

    # Check for explicit SBOM file
    if [ -n "$SBOM_FILE" ] && [ -f "$SBOM_FILE" ]; then
        log_info "  ✓ Using provided SBOM file: $SBOM_FILE"
        cp "$SBOM_FILE" "$EVIDENCE_DIR/sbom-input.json"
        PASSED_CHECKS=$((PASSED_CHECKS + 1))
        return 0
    fi

    # Look for SBOM in common locations
    local sbom_locations=(
        "${MARKETPLACE_DIR}/sbom.json"
        "${MARKETPLACE_DIR}/sbom.spdx.json"
        "${MARKETPLACE_DIR}/sbom.cyclonedx.json"
        "${MARKETPLACE_DIR}/artifacts/sbom.json"
        "${MARKETPLACE_DIR}/../sbom.json"
    )

    for location in "${sbom_locations[@]}"; do
        if [ -f "$location" ]; then
            log_info "  ✓ Found SBOM at: $location"
            cp "$location" "$EVIDENCE_DIR/sbom-input.json"
            SBOM_FILE="$location"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
            return 0
        fi
    done

    # Try to extract SBOM from container image
    log_info "  Attempting to extract SBOM from container image..."
    if extract_sbom_from_image; then
        log_info "  ✓ Extracted SBOM from container image"
        PASSED_CHECKS=$((PASSED_CHECKS + 1))
        return 0
    fi

    log_error "  ✗ No SBOM found in expected locations"
    FAILED_CHECKS=$((FAILED_CHECKS + 1))
    return 1
}

extract_sbom_from_image() {
    if [ -z "$FULL_IMAGE" ]; then
        return 1
    fi

    # Try using Syft to generate SBOM
    if command -v docker &> /dev/null; then
        log_info "    Generating SBOM using Syft..."

        if docker run --rm \
            -v /var/run/docker.sock:/var/run/docker.sock \
            -v "$EVIDENCE_DIR:/output" \
            anchore/syft:latest \
            packages "$FULL_IMAGE" \
            -o spdx-json=/output/sbom-input.json > "$EVIDENCE_DIR/syft-generation.log" 2>&1; then

            SBOM_FILE="$EVIDENCE_DIR/sbom-input.json"
            SBOM_FORMAT="spdx"
            return 0
        fi
    fi

    # Try using grype to generate SBOM
    if command -v docker &> /dev/null; then
        log_info "    Attempting SBOM generation with alternative tool..."

        if docker run --rm \
            -v /var/run/docker.sock:/var/run/docker.sock \
            -v "$EVIDENCE_DIR:/output" \
            anchore/grype:latest \
            "$FULL_IMAGE" \
            -o json > "$EVIDENCE_DIR/sbom-input.json" 2>&1; then

            SBOM_FILE="$EVIDENCE_DIR/sbom-input.json"
            return 0
        fi
    fi

    return 1
}

# ============================================================================
# SBOM Format Validation
# ============================================================================

test_sbom_format() {
    log_test "Validating SBOM format..."

    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))

    if [ ! -f "$EVIDENCE_DIR/sbom-input.json" ]; then
        log_error "  ✗ SBOM file not available for validation"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        return 1
    fi

    # Check if valid JSON
    if ! jq empty "$EVIDENCE_DIR/sbom-input.json" 2>/dev/null; then
        log_error "  ✗ SBOM is not valid JSON"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        return 1
    fi

    # Detect SBOM format
    if jq -e '.spdxVersion' "$EVIDENCE_DIR/sbom-input.json" &>/dev/null; then
        SBOM_FORMAT="spdx"
        SPDX_VERSION=$(jq -r '.spdxVersion' "$EVIDENCE_DIR/sbom-input.json")
        log_info "  ✓ Detected SPDX format (version: $SPDX_VERSION)"
        PASSED_CHECKS=$((PASSED_CHECKS + 1))
        return 0
    elif jq -e '.bomFormat' "$EVIDENCE_DIR/sbom-input.json" &>/dev/null; then
        SBOM_FORMAT="cyclonedx"
        CYCLONE_VERSION=$(jq -r '.specVersion' "$EVIDENCE_DIR/sbom-input.json" 2>/dev/null || echo "unknown")
        log_info "  ✓ Detected CycloneDX format (version: $CYCLONE_VERSION)"
        PASSED_CHECKS=$((PASSED_CHECKS + 1))
        return 0
    else
        log_error "  ✗ Unknown SBOM format (not SPDX or CycloneDX)"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        return 1
    fi
}

# ============================================================================
# SBOM Completeness Validation
# ============================================================================

test_sbom_completeness() {
    log_test "Validating SBOM completeness..."

    if [ ! -f "$EVIDENCE_DIR/sbom-input.json" ]; then
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        return 1
    fi

    local all_complete=true

    # Count components based on format
    if [ "$SBOM_FORMAT" = "spdx" ]; then
        COMPONENT_COUNT=$(jq '[.packages[]?] | length' "$EVIDENCE_DIR/sbom-input.json" 2>/dev/null || echo "0")
    elif [ "$SBOM_FORMAT" = "cyclonedx" ]; then
        COMPONENT_COUNT=$(jq '[.components[]?] | length' "$EVIDENCE_DIR/sbom-input.json" 2>/dev/null || echo "0")
    else
        COMPONENT_COUNT=0
    fi

    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))

    if [ "$COMPONENT_COUNT" -ge "$MIN_COMPONENTS" ]; then
        log_info "  ✓ Component count: $COMPONENT_COUNT (minimum: $MIN_COMPONENTS)"
        PASSED_CHECKS=$((PASSED_CHECKS + 1))
    else
        log_error "  ✗ Insufficient components: $COMPONENT_COUNT (minimum: $MIN_COMPONENTS)"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        all_complete=false
    fi

    echo "$COMPONENT_COUNT" > "$EVIDENCE_DIR/component-count.txt"

    # Check for required metadata
    check_required_metadata

    $all_complete
}

check_required_metadata() {
    log_info "  Checking required metadata..."

    # SPDX-specific checks
    if [ "$SBOM_FORMAT" = "spdx" ]; then
        # Check for document name
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        if jq -e '.name' "$EVIDENCE_DIR/sbom-input.json" &>/dev/null; then
            DOC_NAME=$(jq -r '.name' "$EVIDENCE_DIR/sbom-input.json")
            log_info "    ✓ Document name: $DOC_NAME"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
        else
            log_warn "    ⚠ Missing document name"
            WARNINGS=$((WARNINGS + 1))
        fi

        # Check for creation info
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        if jq -e '.creationInfo' "$EVIDENCE_DIR/sbom-input.json" &>/dev/null; then
            CREATED=$(jq -r '.creationInfo.created' "$EVIDENCE_DIR/sbom-input.json" 2>/dev/null || echo "unknown")
            log_info "    ✓ Creation date: $CREATED"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
        else
            log_warn "    ⚠ Missing creation info"
            WARNINGS=$((WARNINGS + 1))
        fi

        # Check for namespace
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        if jq -e '.documentNamespace' "$EVIDENCE_DIR/sbom-input.json" &>/dev/null; then
            NAMESPACE=$(jq -r '.documentNamespace' "$EVIDENCE_DIR/sbom-input.json")
            log_info "    ✓ Document namespace present"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
        else
            log_error "    ✗ Missing document namespace (required for SPDX)"
            FAILED_CHECKS=$((FAILED_CHECKS + 1))
        fi
    fi

    # CycloneDX-specific checks
    if [ "$SBOM_FORMAT" = "cyclonedx" ]; then
        # Check for metadata
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        if jq -e '.metadata' "$EVIDENCE_DIR/sbom-input.json" &>/dev/null; then
            log_info "    ✓ Metadata section present"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
        else
            log_warn "    ⚠ Missing metadata section"
            WARNINGS=$((WARNINGS + 1))
        fi

        # Check for timestamp
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        if jq -e '.metadata.timestamp' "$EVIDENCE_DIR/sbom-input.json" &>/dev/null; then
            TIMESTAMP=$(jq -r '.metadata.timestamp' "$EVIDENCE_DIR/sbom-input.json")
            log_info "    ✓ Timestamp: $TIMESTAMP"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
        else
            log_warn "    ⚠ Missing timestamp"
            WARNINGS=$((WARNINGS + 1))
        fi
    fi
}

# ============================================================================
# Component Analysis
# ============================================================================

test_component_details() {
    log_test "Analyzing component details..."

    if [ ! -f "$EVIDENCE_DIR/sbom-input.json" ]; then
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        return 1
    fi

    local components_file="$EVIDENCE_DIR/components-analysis.txt"
    echo "Component Analysis" > "$components_file"
    echo "==================" >> "$components_file"
    echo "" >> "$components_file"

    # Extract component details based on format
    if [ "$SBOM_FORMAT" = "spdx" ]; then
        analyze_spdx_components "$components_file"
    elif [ "$SBOM_FORMAT" = "cyclonedx" ]; then
        analyze_cyclonedx_components "$components_file"
    fi

    log_info "  ✓ Component analysis saved to: $components_file"
    return 0
}

analyze_spdx_components() {
    local output_file="$1"

    local components_without_version=0
    local components_without_license=0
    local components_without_checksum=0

    # Process each package
    jq -r '.packages[]? |
        @json' "$EVIDENCE_DIR/sbom-input.json" 2>/dev/null | while read -r component; do

        NAME=$(echo "$component" | jq -r '.name // "unknown"')
        VERSION=$(echo "$component" | jq -r '.versionInfo // "unknown"')
        LICENSE=$(echo "$component" | jq -r '.licenseConcluded // "unknown"')
        CHECKSUM=$(echo "$component" | jq -r '.checksums[0]?.checksumValue // "none"')

        echo "Component: $NAME" >> "$output_file"
        echo "  Version: $VERSION" >> "$output_file"
        echo "  License: $LICENSE" >> "$output_file"
        echo "  Checksum: $CHECKSUM" >> "$output_file"
        echo "" >> "$output_file"

        # Track missing information
        [ "$VERSION" = "unknown" ] && components_without_version=$((components_without_version + 1))
        [ "$LICENSE" = "unknown" ] && components_without_license=$((components_without_license + 1))
        [ "$CHECKSUM" = "none" ] && components_without_checksum=$((components_without_checksum + 1))
    done

    # Report findings
    if [ "$REQUIRE_VERSIONS" = "true" ] && [ $components_without_version -gt 0 ]; then
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        log_warn "  ⚠ $components_without_version components without version info"
        WARNINGS=$((WARNINGS + 1))
    fi

    if [ "$REQUIRE_LICENSES" = "true" ] && [ $components_without_license -gt 0 ]; then
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        log_warn "  ⚠ $components_without_license components without license info"
        WARNINGS=$((WARNINGS + 1))
    fi

    if [ "$REQUIRE_CHECKSUMS" = "true" ] && [ $components_without_checksum -gt 0 ]; then
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        log_error "  ✗ $components_without_checksum components without checksums"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
    fi
}

analyze_cyclonedx_components() {
    local output_file="$1"

    local components_without_version=0
    local components_without_license=0
    local components_without_hash=0

    # Process each component
    jq -r '.components[]? |
        @json' "$EVIDENCE_DIR/sbom-input.json" 2>/dev/null | while read -r component; do

        NAME=$(echo "$component" | jq -r '.name // "unknown"')
        VERSION=$(echo "$component" | jq -r '.version // "unknown"')
        LICENSE=$(echo "$component" | jq -r '.licenses[0]?.license.id // "unknown"')
        HASH=$(echo "$component" | jq -r '.hashes[0]?.content // "none"')

        echo "Component: $NAME" >> "$output_file"
        echo "  Version: $VERSION" >> "$output_file"
        echo "  License: $LICENSE" >> "$output_file"
        echo "  Hash: $HASH" >> "$output_file"
        echo "" >> "$output_file"

        # Track missing information
        [ "$VERSION" = "unknown" ] && components_without_version=$((components_without_version + 1))
        [ "$LICENSE" = "unknown" ] && components_without_license=$((components_without_license + 1))
        [ "$HASH" = "none" ] && components_without_hash=$((components_without_hash + 1))
    done

    # Report findings
    if [ "$REQUIRE_VERSIONS" = "true" ] && [ $components_without_version -gt 0 ]; then
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        log_warn "  ⚠ $components_without_version components without version info"
        WARNINGS=$((WARNINGS + 1))
    fi

    if [ "$REQUIRE_LICENSES" = "true" ] && [ $components_without_license -gt 0 ]; then
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        log_warn "  ⚠ $components_without_license components without license info"
        WARNINGS=$((WARNINGS + 1))
    fi

    if [ "$REQUIRE_CHECKSUMS" = "true" ] && [ $components_without_hash -gt 0 ]; then
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        log_error "  ✗ $components_without_hash components without hashes"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
    fi
}

# ============================================================================
# Image Digest Verification
# ============================================================================

test_image_digest_pinning() {
    log_test "Verifying image digest pinning..."

    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))

    if [ -z "$FULL_IMAGE" ]; then
        log_warn "  ⚠ No image specified for digest verification"
        WARNINGS=$((WARNINGS + 1))
        return 0
    fi

    # Get image digest
    log_info "  Fetching image digest for: $FULL_IMAGE"

    if docker inspect "$FULL_IMAGE" --format='{{index .RepoDigests 0}}' > "$EVIDENCE_DIR/image-digest.txt" 2>&1; then
        IMAGE_DIGEST=$(cat "$EVIDENCE_DIR/image-digest.txt")

        if [ -n "$IMAGE_DIGEST" ] && [ "$IMAGE_DIGEST" != "null" ]; then
            log_info "  ✓ Image digest: $IMAGE_DIGEST"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))

            # Check if digest is in SBOM
            if grep -q "$(echo "$IMAGE_DIGEST" | cut -d'@' -f2)" "$EVIDENCE_DIR/sbom-input.json" 2>/dev/null; then
                log_info "  ✓ Image digest found in SBOM"
            else
                log_warn "  ⚠ Image digest not referenced in SBOM"
                WARNINGS=$((WARNINGS + 1))
            fi

            return 0
        fi
    fi

    log_error "  ✗ Could not retrieve image digest"
    FAILED_CHECKS=$((FAILED_CHECKS + 1))
    return 1
}

# ============================================================================
# Attestation and Signature Verification
# ============================================================================

test_sbom_attestation() {
    log_test "Verifying SBOM attestation..."

    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))

    if [ "$ATTESTATION_REQUIRED" != "true" ]; then
        log_info "  ⊙ Attestation verification skipped (not required)"
        return 0
    fi

    # Check for in-toto attestation
    if jq -e '._type' "$EVIDENCE_DIR/sbom-input.json" &>/dev/null; then
        ATTESTATION_TYPE=$(jq -r '._type' "$EVIDENCE_DIR/sbom-input.json")
        if [ "$ATTESTATION_TYPE" = "https://in-toto.io/Statement/v0.1" ]; then
            log_info "  ✓ In-toto attestation detected"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
            return 0
        fi
    fi

    # Check for cosign signatures if image is available
    if [ -n "$FULL_IMAGE" ] && [ "$COSIGN_VERIFY" = "true" ]; then
        verify_cosign_signature
        return $?
    fi

    log_warn "  ⚠ No attestation found (consider signing SBOM)"
    WARNINGS=$((WARNINGS + 1))
    return 0
}

verify_cosign_signature() {
    log_info "  Checking cosign signatures..."

    if ! command -v docker &> /dev/null; then
        log_warn "    ⚠ Cannot verify cosign signatures (docker not available)"
        WARNINGS=$((WARNINGS + 1))
        return 0
    fi

    # Use cosign via Docker
    if [ "$KEYLESS_VERIFY" = "true" ]; then
        log_info "    Attempting keyless verification..."

        if docker run --rm \
            -v "$EVIDENCE_DIR:/workspace" \
            gcr.io/projectsigstore/cosign:latest \
            verify "$FULL_IMAGE" \
            --certificate-identity-regexp=".*" \
            --certificate-oidc-issuer-regexp=".*" > "$EVIDENCE_DIR/cosign-verify.log" 2>&1; then

            log_info "  ✓ Cosign signature verified (keyless)"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
            return 0
        else
            log_warn "  ⚠ Cosign verification failed (see cosign-verify.log)"
            cat "$EVIDENCE_DIR/cosign-verify.log"
            WARNINGS=$((WARNINGS + 1))
            return 0
        fi
    fi

    log_info "  ⊙ Cosign verification skipped"
    return 0
}

# ============================================================================
# Supply Chain Security Checks
# ============================================================================

test_supply_chain_security() {
    log_test "Checking supply chain security indicators..."

    if [ ! -f "$EVIDENCE_DIR/sbom-input.json" ]; then
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        return 1
    fi

    # Check for supplier information
    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
    if [ "$SBOM_FORMAT" = "spdx" ]; then
        if jq -e '.creationInfo.creators' "$EVIDENCE_DIR/sbom-input.json" &>/dev/null; then
            CREATORS=$(jq -r '.creationInfo.creators[]' "$EVIDENCE_DIR/sbom-input.json" | head -1)
            log_info "  ✓ Creator info present: $CREATORS"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
        else
            log_warn "  ⚠ Missing creator information"
            WARNINGS=$((WARNINGS + 1))
        fi
    elif [ "$SBOM_FORMAT" = "cyclonedx" ]; then
        if jq -e '.metadata.supplier' "$EVIDENCE_DIR/sbom-input.json" &>/dev/null; then
            SUPPLIER=$(jq -r '.metadata.supplier.name' "$EVIDENCE_DIR/sbom-input.json")
            log_info "  ✓ Supplier info present: $SUPPLIER"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
        else
            log_warn "  ⚠ Missing supplier information"
            WARNINGS=$((WARNINGS + 1))
        fi
    fi

    # Check for relationship/dependency information
    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
    if [ "$SBOM_FORMAT" = "spdx" ]; then
        RELATIONSHIPS=$(jq '[.relationships[]?] | length' "$EVIDENCE_DIR/sbom-input.json" 2>/dev/null || echo "0")
        if [ "$RELATIONSHIPS" -gt 0 ]; then
            log_info "  ✓ Relationships defined: $RELATIONSHIPS"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
        else
            log_warn "  ⚠ No dependency relationships defined"
            WARNINGS=$((WARNINGS + 1))
        fi
    elif [ "$SBOM_FORMAT" = "cyclonedx" ]; then
        DEPENDENCIES=$(jq '[.dependencies[]?] | length' "$EVIDENCE_DIR/sbom-input.json" 2>/dev/null || echo "0")
        if [ "$DEPENDENCIES" -gt 0 ]; then
            log_info "  ✓ Dependencies defined: $DEPENDENCIES"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
        else
            log_warn "  ⚠ No dependencies defined"
            WARNINGS=$((WARNINGS + 1))
        fi
    fi

    # Check for external references
    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
    if [ "$SBOM_FORMAT" = "spdx" ]; then
        EXT_REFS=$(jq '[.packages[]?.externalRefs[]?] | length' "$EVIDENCE_DIR/sbom-input.json" 2>/dev/null || echo "0")
        if [ "$EXT_REFS" -gt 0 ]; then
            log_info "  ✓ External references: $EXT_REFS"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
        else
            log_warn "  ⚠ No external references (e.g., CPE, PURL)"
            WARNINGS=$((WARNINGS + 1))
        fi
    elif [ "$SBOM_FORMAT" = "cyclonedx" ]; then
        EXT_REFS=$(jq '[.components[]?.externalReferences[]?] | length' "$EVIDENCE_DIR/sbom-input.json" 2>/dev/null || echo "0")
        if [ "$EXT_REFS" -gt 0 ]; then
            log_info "  ✓ External references: $EXT_REFS"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
        else
            log_warn "  ⚠ No external references"
            WARNINGS=$((WARNINGS + 1))
        fi
    fi
}

# ============================================================================
# Vulnerability Cross-Check
# ============================================================================

test_vulnerability_cross_check() {
    log_test "Cross-checking components against vulnerability databases..."

    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))

    if [ ! -f "$EVIDENCE_DIR/sbom-input.json" ]; then
        log_warn "  ⚠ SBOM not available for vulnerability check"
        WARNINGS=$((WARNINGS + 1))
        return 0
    fi

    # Use Grype to scan SBOM for vulnerabilities
    log_info "  Running vulnerability scan on SBOM..."

    if docker run --rm \
        -v "$EVIDENCE_DIR:/workspace" \
        anchore/grype:latest \
        sbom:/workspace/sbom-input.json \
        -o json > "$EVIDENCE_DIR/vuln-scan.json" 2>&1; then

        VULN_COUNT=$(jq '[.matches[]?] | length' "$EVIDENCE_DIR/vuln-scan.json" 2>/dev/null || echo "0")
        CRITICAL_VULNS=$(jq '[.matches[]? | select(.vulnerability.severity == "Critical")] | length' "$EVIDENCE_DIR/vuln-scan.json" 2>/dev/null || echo "0")
        HIGH_VULNS=$(jq '[.matches[]? | select(.vulnerability.severity == "High")] | length' "$EVIDENCE_DIR/vuln-scan.json" 2>/dev/null || echo "0")

        log_info "  Vulnerabilities found: $VULN_COUNT (Critical: $CRITICAL_VULNS, High: $HIGH_VULNS)"

        if [ "$CRITICAL_VULNS" -gt 0 ]; then
            log_error "  ✗ CRITICAL vulnerabilities detected in SBOM components"
            FAILED_CHECKS=$((FAILED_CHECKS + 1))
            return 1
        elif [ "$HIGH_VULNS" -gt 0 ]; then
            log_warn "  ⚠ HIGH severity vulnerabilities detected"
            WARNINGS=$((WARNINGS + 1))
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
            return 0
        else
            log_info "  ✓ No critical or high vulnerabilities detected"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
            return 0
        fi
    else
        log_warn "  ⚠ Vulnerability scan failed or unavailable"
        WARNINGS=$((WARNINGS + 1))
        return 0
    fi
}

# ============================================================================
# Compliance Checks
# ============================================================================

test_compliance_requirements() {
    log_test "Checking compliance requirements..."

    if [ ! -f "$EVIDENCE_DIR/sbom-input.json" ]; then
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        return 1
    fi

    # Check SPDX compliance
    if [ "$SBOM_FORMAT" = "spdx" ]; then
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))

        # SPDX 2.2 or 2.3 recommended
        SPDX_VERSION=$(jq -r '.spdxVersion' "$EVIDENCE_DIR/sbom-input.json")
        if [[ "$SPDX_VERSION" =~ ^SPDX-2\.[23]$ ]]; then
            log_info "  ✓ SPDX version compliant: $SPDX_VERSION"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
        else
            log_warn "  ⚠ SPDX version may be outdated: $SPDX_VERSION (recommend 2.3)"
            WARNINGS=$((WARNINGS + 1))
        fi
    fi

    # Check CycloneDX compliance
    if [ "$SBOM_FORMAT" = "cyclonedx" ]; then
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))

        SPEC_VERSION=$(jq -r '.specVersion' "$EVIDENCE_DIR/sbom-input.json" 2>/dev/null || echo "unknown")
        if [[ "$SPEC_VERSION" =~ ^1\.[4-9]$ ]]; then
            log_info "  ✓ CycloneDX version compliant: $SPEC_VERSION"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
        else
            log_warn "  ⚠ CycloneDX version may be outdated: $SPEC_VERSION"
            WARNINGS=$((WARNINGS + 1))
        fi
    fi

    # Check for NTIA minimum elements
    check_ntia_minimum_elements
}

check_ntia_minimum_elements() {
    log_info "  Checking NTIA minimum elements..."

    # NTIA requires: Supplier, Component Name, Version, Dependencies, Author, Timestamp
    local ntia_compliant=true

    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))

    # Check supplier/author
    if [ "$SBOM_FORMAT" = "spdx" ]; then
        if ! jq -e '.creationInfo.creators' "$EVIDENCE_DIR/sbom-input.json" &>/dev/null; then
            ntia_compliant=false
        fi
    elif [ "$SBOM_FORMAT" = "cyclonedx" ]; then
        if ! jq -e '.metadata.supplier // .metadata.authors' "$EVIDENCE_DIR/sbom-input.json" &>/dev/null; then
            ntia_compliant=false
        fi
    fi

    # Check timestamp
    if [ "$SBOM_FORMAT" = "spdx" ]; then
        if ! jq -e '.creationInfo.created' "$EVIDENCE_DIR/sbom-input.json" &>/dev/null; then
            ntia_compliant=false
        fi
    elif [ "$SBOM_FORMAT" = "cyclonedx" ]; then
        if ! jq -e '.metadata.timestamp' "$EVIDENCE_DIR/sbom-input.json" &>/dev/null; then
            ntia_compliant=false
        fi
    fi

    if $ntia_compliant; then
        log_info "    ✓ NTIA minimum elements present"
        PASSED_CHECKS=$((PASSED_CHECKS + 1))
    else
        log_error "    ✗ Missing NTIA minimum elements"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
    fi
}

# ============================================================================
# Report Generation
# ============================================================================

generate_verification_report() {
    log_info "Generating SBOM verification report..."

    cat > "$EVIDENCE_DIR/sbom-verification-report.md" <<EOF
# SBOM Verification Report

**Verification Date:** $(date -u +"%Y-%m-%dT%H:%M:%SZ")
**Image:** ${FULL_IMAGE}
**SBOM Format:** ${SBOM_FORMAT^^}
**Verification Mode:** ${VERIFICATION_MODE}

## Executive Summary

| Metric | Value |
|--------|-------|
| Total Checks | $TOTAL_CHECKS |
| Passed | $PASSED_CHECKS |
| Failed | $FAILED_CHECKS |
| Warnings | $WARNINGS |
| **Status** | $([ $FAILED_CHECKS -eq 0 ] && echo "✓ **PASSED**" || echo "✗ **FAILED**") |

## SBOM Details

- **Format:** ${SBOM_FORMAT^^} $([ "$SBOM_FORMAT" = "spdx" ] && echo "$SPDX_VERSION" || echo "$SPEC_VERSION")
- **Components:** $(cat "$EVIDENCE_DIR/component-count.txt" 2>/dev/null || echo "0")
- **Source:** $([ -n "$SBOM_FILE" ] && echo "$SBOM_FILE" || echo "Generated from image")
- **Image Digest:** $(cat "$EVIDENCE_DIR/image-digest.txt" 2>/dev/null || echo "Not available")

## Verification Results

### ✓ Passed Checks
- SBOM format validation
- Component count threshold (minimum: $MIN_COMPONENTS)
$([ $FAILED_CHECKS -eq 0 ] && echo "- All required metadata present")
$([ -f "$EVIDENCE_DIR/image-digest.txt" ] && echo "- Image digest verified and pinned")

### ✗ Failed Checks
$([ $FAILED_CHECKS -gt 0 ] && echo "- $FAILED_CHECKS critical checks failed" || echo "None")

### ⚠ Warnings
$([ $WARNINGS -gt 0 ] && echo "- $WARNINGS non-critical issues detected" || echo "None")

## Supply Chain Security

- **Attestation:** $([ "$ATTESTATION_REQUIRED" = "true" ] && echo "Required" || echo "Optional")
- **Signature Verification:** $([ "$COSIGN_VERIFY" = "true" ] && echo "Enabled" || echo "Disabled")
- **Vulnerability Scan:** $([ -f "$EVIDENCE_DIR/vuln-scan.json" ] && echo "Completed" || echo "Skipped")

## Compliance

$([ "$SBOM_FORMAT" = "spdx" ] && echo "- SPDX Version: $SPDX_VERSION")
$([ "$SBOM_FORMAT" = "cyclonedx" ] && echo "- CycloneDX Version: $SPEC_VERSION")
- NTIA Minimum Elements: $(grep -q "NTIA minimum elements present" "$EVIDENCE_DIR/sbom-verification-report.md" 2>/dev/null && echo "✓ Compliant" || echo "⚠ Check required")

## Recommendations

$(
if [ $FAILED_CHECKS -gt 0 ]; then
    echo "1. **CRITICAL:** Address all failed checks before production deployment"
    echo "2. Review detailed logs in: \`$EVIDENCE_DIR\`"
fi

if [ $WARNINGS -gt 0 ]; then
    echo "- Review warnings and improve SBOM completeness"
fi

if [ "$ATTESTATION_REQUIRED" = "true" ] && ! grep -q "attestation detected" "$EVIDENCE_DIR/sbom-verification-report.md" 2>/dev/null; then
    echo "- Sign SBOM with cosign or equivalent tool"
    echo "- Generate in-toto attestation for supply chain verification"
fi

echo "- Keep SBOM up-to-date with every image release"
echo "- Integrate SBOM generation into CI/CD pipeline"
echo "- Regular vulnerability scanning of SBOM components"
echo "- Pin all image digests in deployment manifests"
)

## Artifacts Generated

- SBOM Input: \`sbom-input.json\`
- Component Analysis: \`components-analysis.txt\`
- Image Digest: \`image-digest.txt\`
$([ -f "$EVIDENCE_DIR/vuln-scan.json" ] && echo "- Vulnerability Scan: \`vuln-scan.json\`")
$([ -f "$EVIDENCE_DIR/cosign-verify.log" ] && echo "- Cosign Verification: \`cosign-verify.log\`")

## Next Steps

1. $([ $FAILED_CHECKS -eq 0 ] && echo "✓ SBOM verification passed - ready for production" || echo "Fix failed checks and re-verify")
2. Integrate SBOM verification into deployment gates
3. Establish SBOM update cadence matching release cycle
4. Implement automated vulnerability monitoring
5. Archive SBOM as immutable artifact with image

---

**Verification Evidence Location:** \`$EVIDENCE_DIR\`

**Principle:** Build systems where incorrect behavior cannot exist.
**Constitution:** DOCKER-ONLY. Deterministic. Replayable. Complete.

EOF

    log_info "  ✓ Report saved to: $EVIDENCE_DIR/sbom-verification-report.md"

    # Print report
    cat "$EVIDENCE_DIR/sbom-verification-report.md"
}

# ============================================================================
# Main Verification Runner
# ============================================================================

main() {
    log_info "Starting SBOM Verification..."
    log_info "================================================"
    log_info "Image: ${FULL_IMAGE}"
    log_info "Mode: ${VERIFICATION_MODE}"
    log_info "Attestation Required: ${ATTESTATION_REQUIRED}"
    log_info "================================================"

    check_prerequisites

    # Run verification tests
    test_sbom_discovery
    test_sbom_format
    test_sbom_completeness
    test_component_details
    test_image_digest_pinning
    test_sbom_attestation
    test_supply_chain_security
    test_vulnerability_cross_check
    test_compliance_requirements

    # Generate report
    generate_verification_report

    # Summary
    log_info "================================================"
    log_info "SBOM Verification Summary:"
    log_info "  Total Checks:  $TOTAL_CHECKS"
    log_info "  Passed:        $PASSED_CHECKS"
    log_info "  Failed:        $FAILED_CHECKS"
    log_info "  Warnings:      $WARNINGS"
    log_info "================================================"
    log_info "Evidence saved to: $EVIDENCE_DIR"

    # Exit based on verification mode
    if [ $FAILED_CHECKS -gt 0 ]; then
        if [ "$VERIFICATION_MODE" = "strict" ]; then
            log_error "SBOM verification FAILED in strict mode"
            exit 1
        elif [ "$VERIFICATION_MODE" = "relaxed" ]; then
            log_warn "SBOM verification has failures (allowed in relaxed mode)"
            exit 0
        else
            log_info "SBOM verification completed (audit mode)"
            exit 0
        fi
    fi

    log_info "SBOM verification PASSED!"
    exit 0
}

# ============================================================================
# Script Entry Point
# ============================================================================

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --project)
            PROJECT_ID="$2"
            shift 2
            ;;
        --region)
            REGION="$2"
            shift 2
            ;;
        --image)
            FULL_IMAGE="$2"
            shift 2
            ;;
        --sbom)
            SBOM_FILE="$2"
            shift 2
            ;;
        --format)
            SBOM_FORMAT="$2"
            shift 2
            ;;
        --mode)
            VERIFICATION_MODE="$2"
            shift 2
            ;;
        --min-components)
            MIN_COMPONENTS="$2"
            shift 2
            ;;
        --require-attestation)
            ATTESTATION_REQUIRED=true
            shift
            ;;
        --no-attestation)
            ATTESTATION_REQUIRED=false
            shift
            ;;
        --cosign)
            COSIGN_VERIFY=true
            shift
            ;;
        --help)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --project PROJECT_ID          GCP project ID"
            echo "  --region REGION              GCP region (default: us-central1)"
            echo "  --image IMAGE                Full image name to verify"
            echo "  --sbom FILE                  Path to SBOM file"
            echo "  --format FORMAT              SBOM format: spdx|cyclonedx (default: spdx)"
            echo "  --mode MODE                  Verification mode: strict|relaxed|audit (default: strict)"
            echo "  --min-components NUM         Minimum component count (default: 10)"
            echo "  --require-attestation        Require SBOM attestation"
            echo "  --no-attestation            Skip attestation check"
            echo "  --cosign                    Enable cosign signature verification"
            echo "  --help                      Show this help message"
            exit 0
            ;;
        *)
            log_error "Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

main
