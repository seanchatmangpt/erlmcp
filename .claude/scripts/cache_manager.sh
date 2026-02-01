#!/bin/bash
# Cache management utilities for erlmcp cloud installations
# Manages OTP binaries, rebar3, and hex package caches

set -euo pipefail

CACHE_DIR="${ERLMCP_CACHE:-$HOME/.erlmcp/cache}"
OTP_DIR="${ERLMCP_OTP:-$HOME/.erlmcp/otp}"

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

# ============================================================================
# Cache Functions
# ============================================================================

cache_otp() {
    local otp_version="$1"
    local install_dir="$2"

    echo -e "${BLUE}Caching OTP $otp_version...${NC}"

    if [ ! -d "$install_dir" ]; then
        echo -e "${RED}✗${NC} Installation directory not found: $install_dir"
        return 1
    fi

    mkdir -p "$CACHE_DIR"

    cd "$(dirname "$install_dir")"
    tar -czf "$CACHE_DIR/otp-$otp_version.tar.gz" "$(basename "$install_dir")"

    # Generate checksum
    cd "$CACHE_DIR"
    sha256sum "otp-$otp_version.tar.gz" >> checksums.txt

    echo -e "${GREEN}✓${NC} OTP $otp_version cached to $CACHE_DIR"
}

cache_rebar3() {
    local rebar3_version="$1"
    local rebar3_bin="$2"

    echo -e "${BLUE}Caching rebar3 $rebar3_version...${NC}"

    if [ ! -f "$rebar3_bin" ]; then
        echo -e "${RED}✗${NC} rebar3 binary not found: $rebar3_bin"
        return 1
    fi

    mkdir -p "$CACHE_DIR"

    cp "$rebar3_bin" "$CACHE_DIR/rebar3-$rebar3_version"

    # Generate checksum
    cd "$CACHE_DIR"
    sha256sum "rebar3-$rebar3_version" >> checksums.txt

    echo -e "${GREEN}✓${NC} rebar3 $rebar3_version cached"
}

cache_deps() {
    local version="$1"

    echo -e "${BLUE}Caching hex dependencies for erlmcp $version...${NC}"

    if [ ! -d "_build/default/lib" ]; then
        echo -e "${RED}✗${NC} Dependencies not compiled yet"
        echo "  Run 'rebar3 get-deps' first"
        return 1
    fi

    mkdir -p "$CACHE_DIR"

    tar -czf "$CACHE_DIR/deps-$version.tar.gz" \
        _build/default/lib \
        ~/.cache/rebar3 2>/dev/null || true

    # Generate checksum
    cd "$CACHE_DIR"
    sha256sum "deps-$version.tar.gz" >> checksums.txt

    echo -e "${GREEN}✓${NC} Dependencies cached"
}

verify_cache() {
    local file="$1"

    if [ ! -f "$file" ]; then
        echo -e "${RED}✗${NC} Cache file not found: $file"
        return 1
    fi

    # Verify checksum if available
    if [ -f "$CACHE_DIR/checksums.txt" ]; then
        local basename=$(basename "$file")

        if grep -q "$basename" "$CACHE_DIR/checksums.txt"; then
            echo -e "${BLUE}→${NC} Verifying checksum for $basename..."

            cd "$CACHE_DIR"
            if sha256sum -c <(grep "$basename" checksums.txt) >/dev/null 2>&1; then
                echo -e "${GREEN}✓${NC} Checksum verified"
                return 0
            else
                echo -e "${RED}✗${NC} Checksum mismatch - cache corrupted"
                return 1
            fi
        fi
    fi

    echo -e "${YELLOW}⚠${NC} No checksum available, skipping verification"
    return 0
}

clean_cache() {
    local max_age_days="${1:-30}"

    echo -e "${BLUE}Cleaning cache (files older than $max_age_days days)...${NC}"

    if [ ! -d "$CACHE_DIR" ]; then
        echo -e "${YELLOW}⚠${NC} Cache directory does not exist"
        return 0
    fi

    local deleted_count=0
    while IFS= read -r file; do
        echo "  Deleting: $file"
        rm -f "$file"
        deleted_count=$((deleted_count + 1))
    done < <(find "$CACHE_DIR" -type f -mtime +$max_age_days 2>/dev/null || true)

    if [ $deleted_count -eq 0 ]; then
        echo -e "${GREEN}✓${NC} No old files to clean"
    else
        echo -e "${GREEN}✓${NC} Deleted $deleted_count file(s)"
    fi
}

cache_stats() {
    echo "Cache Statistics"
    echo "════════════════════════════════════════════════"
    echo "Location: $CACHE_DIR"
    echo ""

    if [ ! -d "$CACHE_DIR" ]; then
        echo -e "${YELLOW}Cache directory does not exist${NC}"
        return 0
    fi

    echo "Cached files:"
    echo ""

    # List OTP caches
    local otp_count=0
    while IFS= read -r file; do
        local size=$(du -sh "$file" | cut -f1)
        local name=$(basename "$file")
        local age=$(find "$file" -printf '%TY-%Tm-%Td\n')
        echo "  $name ($size, cached: $age)"
        otp_count=$((otp_count + 1))
    done < <(find "$CACHE_DIR" -name "otp-*.tar.gz" 2>/dev/null || true)

    if [ $otp_count -eq 0 ]; then
        echo "  (no OTP caches)"
    fi

    echo ""

    # List rebar3 caches
    local rebar3_count=0
    while IFS= read -r file; do
        local size=$(du -sh "$file" | cut -f1)
        local name=$(basename "$file")
        local age=$(find "$file" -printf '%TY-%Tm-%Td\n')
        echo "  $name ($size, cached: $age)"
        rebar3_count=$((rebar3_count + 1))
    done < <(find "$CACHE_DIR" -name "rebar3-*" 2>/dev/null || true)

    if [ $rebar3_count -eq 0 ]; then
        echo "  (no rebar3 caches)"
    fi

    echo ""

    # List dependency caches
    local deps_count=0
    while IFS= read -r file; do
        local size=$(du -sh "$file" | cut -f1)
        local name=$(basename "$file")
        local age=$(find "$file" -printf '%TY-%Tm-%Td\n')
        echo "  $name ($size, cached: $age)"
        deps_count=$((deps_count + 1))
    done < <(find "$CACHE_DIR" -name "deps-*.tar.gz" 2>/dev/null || true)

    if [ $deps_count -eq 0 ]; then
        echo "  (no dependency caches)"
    fi

    echo ""
    echo "Total size: $(du -sh "$CACHE_DIR" 2>/dev/null | cut -f1 || echo "0")"
    echo "Total files: $((otp_count + rebar3_count + deps_count))"
    echo ""

    # Show checksums if available
    if [ -f "$CACHE_DIR/checksums.txt" ]; then
        echo "Checksums: $(wc -l < "$CACHE_DIR/checksums.txt") entries"
    else
        echo "Checksums: none"
    fi
}

clear_cache() {
    echo -e "${YELLOW}⚠${NC} This will delete all cached files"
    read -p "Are you sure? (y/N) " -n 1 -r
    echo

    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Cancelled"
        return 1
    fi

    if [ -d "$CACHE_DIR" ]; then
        rm -rf "$CACHE_DIR"
        echo -e "${GREEN}✓${NC} Cache cleared"
    else
        echo -e "${YELLOW}⚠${NC} Cache directory does not exist"
    fi
}

# ============================================================================
# Main Command Dispatch
# ============================================================================

usage() {
    cat << EOF
Usage: $(basename "$0") <command> [args]

Commands:
  stats                        Show cache statistics
  clean [days]                 Clean files older than N days (default: 30)
  clear                        Delete all cached files
  verify <file>                Verify checksum of cached file
  cache-otp <version> <dir>    Cache OTP installation
  cache-rebar3 <version> <bin> Cache rebar3 binary
  cache-deps <version>         Cache hex dependencies

Examples:
  $(basename "$0") stats
  $(basename "$0") clean 30
  $(basename "$0") cache-otp 28.0 ~/.erlmcp/otp/28.0
  $(basename "$0") verify ~/.erlmcp/cache/otp-28.0.tar.gz

Environment:
  ERLMCP_CACHE - Cache directory (default: ~/.erlmcp/cache)
EOF
}

# Command dispatch
COMMAND="${1:-stats}"

case "$COMMAND" in
    stats)
        cache_stats
        ;;
    clean)
        clean_cache "${2:-30}"
        ;;
    clear)
        clear_cache
        ;;
    verify)
        if [ -z "${2:-}" ]; then
            echo "Error: Missing file argument"
            usage
            exit 1
        fi
        verify_cache "$2"
        ;;
    cache-otp)
        if [ -z "${2:-}" ] || [ -z "${3:-}" ]; then
            echo "Error: Missing version or directory"
            usage
            exit 1
        fi
        cache_otp "$2" "$3"
        ;;
    cache-rebar3)
        if [ -z "${2:-}" ] || [ -z "${3:-}" ]; then
            echo "Error: Missing version or binary path"
            usage
            exit 1
        fi
        cache_rebar3 "$2" "$3"
        ;;
    cache-deps)
        if [ -z "${2:-}" ]; then
            echo "Error: Missing version"
            usage
            exit 1
        fi
        cache_deps "$2"
        ;;
    help|--help|-h)
        usage
        ;;
    *)
        echo "Unknown command: $COMMAND"
        echo ""
        usage
        exit 1
        ;;
esac
