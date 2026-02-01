#!/bin/bash
# Network compliance checker for erlmcp
# Ensures all dependencies use allowlisted domains only

set -euo pipefail

ALLOWLIST=(
    "github.com"
    "raw.githubusercontent.com"
    "api.github.com"
    "hex.pm"
    "repo.hex.pm"
    "builds.hex.pm"
    "cdn.jsdelivr.net"
)

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$PROJECT_ROOT"

echo "Network Compliance Audit - erlmcp"
echo "========================================"
echo ""

VIOLATIONS=0
CHECKS=0

check_url() {
    local url="$1"
    local source="$2"

    CHECKS=$((CHECKS + 1))

    # Extract domain from URL
    local domain=$(echo "$url" | sed -E 's|https?://([^/]+).*|\1|')

    # Remove www. prefix if present
    domain=$(echo "$domain" | sed 's/^www\.//')

    # Check if domain is in allowlist
    for allowed in "${ALLOWLIST[@]}"; do
        if [[ "$domain" == "$allowed" || "$domain" == *".$allowed" ]]; then
            echo -e "${GREEN}✓${NC} $source: $url (domain: $domain)"
            return 0
        fi
    done

    echo -e "${RED}✗${NC} $source: $url (domain: $domain) - NOT ALLOWLISTED"
    VIOLATIONS=$((VIOLATIONS + 1))
    return 1
}

echo -e "${BLUE}Checking rebar.config dependencies...${NC}"
echo ""

# Check all git dependencies in rebar.config
if [ -f "rebar.config" ]; then
    while IFS= read -r line; do
        # Match git URLs in various formats
        if [[ "$line" =~ \{git,.*\"([^\"]+)\" ]]; then
            url="${BASH_REMATCH[1]}"
            check_url "$url" "rebar.config"
        fi
    done < rebar.config
else
    echo -e "${YELLOW}⚠${NC} rebar.config not found"
fi

# Check for any hardcoded URLs in scripts
echo ""
echo -e "${BLUE}Checking scripts for hardcoded URLs...${NC}"
echo ""

SCRIPTS_CHECKED=0

# Check all shell scripts
while IFS= read -r script; do
    SCRIPTS_CHECKED=$((SCRIPTS_CHECKED + 1))

    # Extract URLs from script (http:// or https://)
    urls=$(grep -Eo 'https?://[^"'\'' ]+' "$script" 2>/dev/null | sort -u || true)

    if [ -n "$urls" ]; then
        while IFS= read -r url; do
            # Skip comments
            if grep -q "^[[:space:]]*#.*$url" "$script" 2>/dev/null; then
                continue
            fi

            # Skip localhost URLs
            if [[ "$url" =~ localhost|127\.0\.0\.1 ]]; then
                continue
            fi

            check_url "$url" "$script"
        done <<< "$urls"
    fi
done < <(find scripts .claude -type f -name "*.sh" 2>/dev/null || true)

# Check Makefiles
echo ""
echo -e "${BLUE}Checking Makefiles...${NC}"
echo ""

if [ -f "Makefile" ]; then
    urls=$(grep -Eo 'https?://[^"'\'' ]+' Makefile 2>/dev/null | sort -u || true)

    if [ -n "$urls" ]; then
        while IFS= read -r url; do
            check_url "$url" "Makefile"
        done <<< "$urls"
    fi
fi

# Check GitHub Actions workflows
echo ""
echo -e "${BLUE}Checking GitHub Actions workflows...${NC}"
echo ""

WORKFLOWS_CHECKED=0
if [ -d ".github/workflows" ]; then
    while IFS= read -r workflow; do
        WORKFLOWS_CHECKED=$((WORKFLOWS_CHECKED + 1))

        urls=$(grep -Eo 'https?://[^"'\'' ]+' "$workflow" 2>/dev/null | sort -u || true)

        if [ -n "$urls" ]; then
            while IFS= read -r url; do
                # Skip GitHub Actions uses: directives (they're allowlisted)
                if [[ "$url" =~ github.com.*@v[0-9] ]]; then
                    continue
                fi

                check_url "$url" "$workflow"
            done <<< "$urls"
        fi
    done < <(find .github/workflows -type f -name "*.yml" -o -name "*.yaml" 2>/dev/null || true)
fi

# Summary
echo ""
echo "========================================"
echo "Scripts checked: $SCRIPTS_CHECKED"
echo "Workflows checked: $WORKFLOWS_CHECKED"
echo "Total URL checks: $CHECKS"
echo ""

if [ $VIOLATIONS -eq 0 ]; then
    echo -e "${GREEN}✅ All dependencies use allowlisted domains${NC}"
    echo ""
    echo "Allowlisted domains:"
    for domain in "${ALLOWLIST[@]}"; do
        echo "  - $domain"
    done
    echo ""
    exit 0
else
    echo -e "${RED}❌ Found $VIOLATIONS violation(s)${NC}"
    echo ""
    echo "Action required:"
    echo "  1. Replace blocked URLs with allowlisted alternatives"
    echo "  2. Use GitHub releases instead of external CDNs"
    echo "  3. Update allowlist if new domain is required (requires approval)"
    echo ""
    echo "Allowlisted domains:"
    for domain in "${ALLOWLIST[@]}"; do
        echo "  - $domain"
    done
    echo ""
    echo "To add a new domain to the allowlist:"
    echo "  1. Verify domain is necessary and trustworthy"
    echo "  2. Add to ALLOWLIST array in scripts/check_network_compliance.sh"
    echo "  3. Document in NETWORK_AWARE_SETUP_DESIGN.md"
    echo ""
    exit 1
fi
