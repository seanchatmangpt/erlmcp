#!/usr/bin/env bash
# manual-download.sh - Manual hex package fetching and installation
# Purpose: Download packages directly from hex.pm and install to local cache
# Usage: ./manual-download.sh [--package PACKAGE] [--version VERSION]

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
LOG_DIR="${PROJECT_ROOT}/log/manual-download"

# Hex cache directory (standard rebar3 location)
HEX_HOME="${HOME}/.cache/rebar3/hex/default"
HEX_PACKAGES_DIR="${HEX_HOME}/packages"
HEX_TARBALLS_DIR="${HEX_HOME}/tarballs"

# Hex API
HEX_API="https://hex.pm/api"
HEX_REPO="https://repo.hex.pm"

mkdir -p "${LOG_DIR}" "${HEX_PACKAGES_DIR}" "${HEX_TARBALLS_DIR}"

log() {
    echo -e "${BLUE}[$(date +'%Y-%m-%d %H:%M:%S')]${NC} $*" | tee -a "${LOG_DIR}/download.log"
}

log_success() {
    echo -e "${GREEN}✓${NC} $*" | tee -a "${LOG_DIR}/download.log"
}

log_error() {
    echo -e "${RED}✗${NC} $*" | tee -a "${LOG_DIR}/download.log"
}

log_warning() {
    echo -e "${YELLOW}⚠${NC} $*" | tee -a "${LOG_DIR}/download.log"
}

# Fetch package metadata from hex API
fetch_package_metadata() {
    local package="$1"
    local metadata_file="${LOG_DIR}/${package}-metadata.json"
    
    log "Fetching metadata for ${package}"
    
    if curl -s -f "${HEX_API}/packages/${package}" -o "${metadata_file}"; then
        log_success "Metadata downloaded: ${metadata_file}"
        echo "${metadata_file}"
        return 0
    else
        log_error "Failed to fetch metadata for ${package}"
        return 1
    fi
}

# Get package checksum from metadata
get_package_checksum() {
    local package="$1"
    local version="$2"
    local metadata_file="$3"
    
    # Extract checksum from JSON (using grep/sed for portability)
    local checksum
    checksum=$(grep -o "\"${version}\"[^}]*" "${metadata_file}" | grep -o '"checksum":"[^"]*' | sed 's/"checksum":"//' | head -1)
    
    if [ -n "${checksum}" ]; then
        echo "${checksum}"
        return 0
    else
        log_error "Could not extract checksum for ${package}-${version}"
        return 1
    fi
}

# Download package tarball
download_package() {
    local package="$1"
    local version="$2"
    local output_file="$3"
    
    log "Downloading ${package}-${version}"
    
    # Hex tarball URL format
    local tarball_url="${HEX_REPO}/tarballs/${package}-${version}.tar"
    
    # Download with progress
    if curl -# -f -L -o "${output_file}" "${tarball_url}"; then
        local size=$(stat -f%z "${output_file}" 2>/dev/null || stat -c%s "${output_file}" 2>/dev/null || echo "0")
        local size_kb=$((size / 1024))
        log_success "Downloaded ${package}-${version} (${size_kb}KB)"
        return 0
    else
        log_error "Download failed: ${tarball_url}"
        return 1
    fi
}

# Verify package checksum
verify_checksum() {
    local file="$1"
    local expected_checksum="$2"
    
    log "Verifying checksum"
    
    # Compute SHA256
    local actual_checksum
    if command -v sha256sum >/dev/null 2>&1; then
        actual_checksum=$(sha256sum "${file}" | awk '{print toupper($1)}')
    elif command -v shasum >/dev/null 2>&1; then
        actual_checksum=$(shasum -a 256 "${file}" | awk '{print toupper($1)}')
    else
        log_error "No checksum tool available (sha256sum or shasum)"
        return 1
    fi
    
    if [ "${actual_checksum}" == "${expected_checksum}" ]; then
        log_success "Checksum verified: ${actual_checksum}"
        return 0
    else
        log_error "Checksum mismatch!"
        log_error "  Expected: ${expected_checksum}"
        log_error "  Actual:   ${actual_checksum}"
        return 1
    fi
}

# Extract tarball metadata
extract_tarball_info() {
    local tarball="$1"
    local extract_dir="${LOG_DIR}/extract-$(basename ${tarball} .tar)"
    
    rm -rf "${extract_dir}"
    mkdir -p "${extract_dir}"
    
    log "Extracting tarball metadata"
    
    if tar -xf "${tarball}" -C "${extract_dir}" 2>/dev/null; then
        log_success "Extracted to ${extract_dir}"
        
        # Show contents
        log "Contents:"
        ls -lh "${extract_dir}" | tee -a "${LOG_DIR}/download.log"
        
        # Check for metadata.config
        if [ -f "${extract_dir}/metadata.config" ]; then
            log "metadata.config:"
            cat "${extract_dir}/metadata.config" | tee -a "${LOG_DIR}/download.log"
        fi
        
        return 0
    else
        log_error "Failed to extract tarball"
        return 1
    fi
}

# Install to hex cache
install_to_cache() {
    local package="$1"
    local version="$2"
    local tarball="$3"
    
    log "Installing to hex cache"
    
    # Copy to cache
    local cache_file="${HEX_TARBALLS_DIR}/${package}-${version}.tar"
    cp "${tarball}" "${cache_file}"
    
    log_success "Installed to: ${cache_file}"
    
    # Create package index entry
    local index_file="${HEX_PACKAGES_DIR}/${package}"
    if [ ! -f "${index_file}" ]; then
        echo "{\"${version}\": \"${cache_file}\"}" > "${index_file}"
        log_success "Created index: ${index_file}"
    else
        log_warning "Index already exists: ${index_file}"
    fi
    
    return 0
}

# Download and install single package
download_and_install_package() {
    local package="$1"
    local version="$2"
    
    log "========================================="
    log "Package: ${package}-${version}"
    log "========================================="
    
    # Step 1: Fetch metadata
    local metadata_file
    if ! metadata_file=$(fetch_package_metadata "${package}"); then
        return 1
    fi
    
    # Step 2: Get checksum
    local expected_checksum
    if ! expected_checksum=$(get_package_checksum "${package}" "${version}" "${metadata_file}"); then
        log_warning "Could not get checksum, proceeding without verification"
        expected_checksum=""
    fi
    
    # Step 3: Download
    local tarball="${LOG_DIR}/${package}-${version}.tar"
    if ! download_package "${package}" "${version}" "${tarball}"; then
        return 1
    fi
    
    # Step 4: Verify checksum
    if [ -n "${expected_checksum}" ]; then
        if ! verify_checksum "${tarball}" "${expected_checksum}"; then
            rm -f "${tarball}"
            return 1
        fi
    fi
    
    # Step 5: Extract and inspect
    extract_tarball_info "${tarball}"
    
    # Step 6: Install to cache
    if install_to_cache "${package}" "${version}" "${tarball}"; then
        log_success "Successfully installed ${package}-${version}"
        return 0
    else
        return 1
    fi
}

# Download all dependencies from rebar.lock
download_all_from_lock() {
    local lock_file="${PROJECT_ROOT}/rebar.lock"
    
    if [ ! -f "${lock_file}" ]; then
        log_error "rebar.lock not found: ${lock_file}"
        return 1
    fi
    
    log "Reading dependencies from rebar.lock"
    
    # Parse rebar.lock (Erlang term format)
    # Extract package names and versions
    local packages
    packages=$(grep -o '<<"[^"]*">>,{pkg,<<"[^"]*">>,<<"[^"]*">>}' "${lock_file}" | \
               sed 's/<<"//g; s/">>//g; s/,{pkg,/ /g')
    
    local total=0
    local successful=0
    local failed=0
    
    while IFS= read -r line; do
        if [ -n "${line}" ]; then
            local package=$(echo "${line}" | awk '{print $1}')
            local version=$(echo "${line}" | awk '{print $2}')
            
            ((total++))
            
            echo ""
            if download_and_install_package "${package}" "${version}"; then
                ((successful++))
            else
                ((failed++))
            fi
        fi
    done <<< "${packages}"
    
    echo ""
    log "========================================="
    log "SUMMARY"
    log "========================================="
    log "Total packages: ${total}"
    log_success "Successful: ${successful}"
    if [ ${failed} -gt 0 ]; then
        log_error "Failed: ${failed}"
    fi
}

# Test if rebar3 recognizes manually downloaded packages
test_rebar3_recognition() {
    log "========================================="
    log "Testing rebar3 recognition"
    log "========================================="
    
    cd "${PROJECT_ROOT}"
    
    # Clean build
    log "Cleaning build directory"
    rm -rf _build
    
    # Try to compile (should use cached packages)
    log "Testing rebar3 compile with cached packages"
    
    if TERM=dumb rebar3 compile 2>&1 | tee -a "${LOG_DIR}/rebar3-test.log"; then
        log_success "rebar3 successfully used cached packages"
        return 0
    else
        log_error "rebar3 failed to use cached packages"
        log "Check log: ${LOG_DIR}/rebar3-test.log"
        return 1
    fi
}

# Main
main() {
    log "Starting manual package download"
    log "Hex cache: ${HEX_HOME}"
    
    # Parse arguments
    local mode="all"
    local package=""
    local version=""
    
    while [ $# -gt 0 ]; do
        case "$1" in
            --package)
                package="$2"
                mode="single"
                shift 2
                ;;
            --version)
                version="$2"
                shift 2
                ;;
            *)
                log_error "Unknown option: $1"
                echo "Usage: $0 [--package PACKAGE --version VERSION]"
                exit 1
                ;;
        esac
    done
    
    if [ "${mode}" == "single" ]; then
        if [ -z "${package}" ] || [ -z "${version}" ]; then
            log_error "Both --package and --version required"
            exit 1
        fi
        download_and_install_package "${package}" "${version}"
    else
        download_all_from_lock
        
        echo ""
        read -p "Test rebar3 with cached packages? (y/n) " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            test_rebar3_recognition
        fi
    fi
    
    log "Download completed at $(date)"
    log "Logs: ${LOG_DIR}/download.log"
}

main "$@"
