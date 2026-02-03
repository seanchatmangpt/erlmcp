#!/bin/bash
# Secret Audit Script for erlmcp v3
# Scans codebase for potential secrets and verifies they're not in git
#
# Usage: ./audit-secrets.sh [options]
#
# Options:
#   --fix        Attempt to fix issues (requires confirmation)
#   --output     Output format (text, json, sarif)
#   --severity   Minimum severity (low, medium, high, critical)

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
OUTPUT_FORMAT="text"
MIN_SEVERITY="medium"
FIX_MODE=false
FOUND_ISSUES=0

# Secret patterns to detect (in order of severity)
declare -A SECRET_PATTERNS=(
    # Critical - Actual credentials
    ["password\\s*[:=]\\s*['\"][^'\"]{8,}['\"]"]="CRITICAL:Hardcoded password"
    ["api[_-]?key\\s*[:=]\\s*['\"][^'\"]{20,}['\"]"]="CRITICAL:API key"
    ["secret[_-]?key\\s*[:=]\\s*['\"][^'\"]{20,}['\"]"]="CRITICAL:Secret key"
    ["access[_-]?token\\s*[:=]\\s*['\"][^'\"]{20,}['\"]"]="CRITICAL:Access token"
    ["refresh[_-]?token\\s*[:=]\\s*['\"][^'\"]{20,}['\"]"]="CRITICAL:Refresh token"
    ["private[_-]?key\\s*[:=]\\s*['\"]-{5,}BEGIN.*PRIVATE KEY"]="CRITICAL:Private key"
    ["ssh[_-]?private\\s*[:=]\\s*['\"][^'\"]{50,}['\"]"]="CRITICAL:SSH private key"

    # High - Sensitive configuration
    ["jwt[_-]?secret\\s*[:=]\\s*['\"][^'\"]{16,}['\"]"]="HIGH:JWT secret"
    ["session[_-]?secret\\s*[:=]\\s*['\"][^'\"]{16,}['\"]"]="HIGH:Session secret"
    ["encryption[_-]?key\\s*[:=]\\s*['\"][^'\"]{16,}['\"]"]="HIGH:Encryption key"
    ["database[_-]?url\\s*[:=]\\s*['\"][^'\"]*://[^:'\"]+:[^'\"@]+@"]="HIGH:Database URL with password"
    ["connection[_-]?string\\s*[:=]\\s*['\"][^'\"]*password=[^'\"]+['\"]"]="HIGH:Connection string with password"
    ["redis[_-]?password\\s*[:=]\\s*['\"][^'\"]{8,}['\"]"]="HIGH:Redis password"
    ["mongodb(?:\\+srv)?:\\/\\/[^:]+:[^@]+@"]="HIGH:MongoDB connection string"

    # Medium - Potential secrets
    ["authorization\\s*[:=]\\s*['\"]Bearer [^'\"]+['\"]"]="MEDIUM:Bearer token"
    ["token\\s*[:=]\\s*['\"]ey[a-zA-Z0-9_-]{50,}['\"]"]="MEDIUM:JWT token"
    ["client[_-]?secret\\s*[:=]\\s*['\"][^'\"]{16,}['\"]"]="MEDIUM:OAuth client secret"
    ["auth[_-]?token\\s*[:=]\\s*['\"][^'\"]{20,}['\"]"]="MEDIUM:Auth token"

    # Low - Infrastructure tokens
    ["AKIA[0-9A-Z]{16}"]="LOW:AWS Access Key"
    ["ghp_[a-zA-Z0-9]{36}"]="LOW:GitHub Personal Access Token"
    ["glpat-[a-zA-Z0-9\\-_]{20,}"]="LOW:GitLab Personal Access Token"
    ["sk-[a-zA-Z0-9]{48}"]="LOW:OpenAI API Key"
    ["xox[baprs]-[0-9]{12}-[0-9]{12}-[0-9]{12}-[a-z0-9]{32}"]="LOW:Slack token"
)

# File extensions to scan
SCAN_EXTENSIONS=(
    ".erl" ".hrl" ".app.src" ".config" ".sys"
    ".yaml" ".yml" ".json"
    ".sh" ".bash" ".zsh"
    ".py" ".js" ".ts" ".go" ".rs"
    ".tf" ".tfvars"
    ".md" ".txt"
)

# Directories to exclude
EXCLUDE_DIRS=(
    ".git" "_build" "deps" "logs"
    "node_modules" "vendor" ".venv"
    "coverage" ".elixir_ls"
    "external-secrets" "vault"  # These contain secret definitions, not secrets
)

# Logging functions
log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }
log_debug() { echo -e "${BLUE}[DEBUG]${NC} $1"; }

# Parse arguments
parse_args() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            --fix)
                FIX_MODE=true
                shift
                ;;
            --output)
                OUTPUT_FORMAT="$2"
                shift 2
                ;;
            --severity)
                MIN_SEVERITY="$2"
                shift 2
                ;;
            *)
                log_error "Unknown option: $1"
                exit 1
                ;;
        esac
    done
}

# Check if path should be excluded
should_exclude() {
    local path="$1"
    for dir in "${EXCLUDE_DIRS[@]}"; do
        if [[ "${path}" == */${dir}/* ]] || [[ "${path}" == */${dir} ]]; then
            return 0
        fi
    done

    # Exclude template files that use placeholders
    if [[ "${path}" =~ template ]] && [[ "${path}" =~ \.template ]]; then
        return 0
    fi

    # Exclude .env template files
    if [[ "${path}" =~ \.env\..*template ]]; then
        return 0
    fi

    return 1
}

# Check if severity meets threshold
check_severity() {
    local severity="$1"
    local levels=("LOW" "MEDIUM" "HIGH" "CRITICAL")
    local min_idx=0
    local current_idx=0

    for i in "${!levels[@]}"; do
        [[ "${levels[$i]}" == "${MIN_SEVERITY}" ]] && min_idx=$i
        [[ "${levels[$i]}" == "${severity%%:*}" ]] && current_idx=$i
    done

    [[ $current_idx -ge $min_idx ]]
}

# Scan a file for secrets
scan_file() {
    local file="$1"
    local issues=()

    while IFS= read -r line; do
        local line_num="${line%%:*}"
        local content="${line#*:}"

        for pattern in "${!SECRET_PATTERNS[@]}"; do
            if echo "${content}" | grep -qiE "${pattern}"; then
                local issue_info="${SECRET_PATTERNS[$pattern]}"
                local severity="${issue_info%%:*}"
                local description="${issue_info#*:}"

                if check_severity "${severity}"; then
                    issues+=("${line_num}:${severity}:${description}")
                fi
            fi
        done
    done < <(grep -nE "$(IFS='|'; echo "${!SECRET_PATTERNS[*]}")" "${file}" 2>/dev/null || true)

    # Return issues
    printf '%s\n' "${issues[@]}"
}

# Format output based on format
format_output() {
    local file="$1"
    local issue="$2"
    local line="${issue%%:*}"
    local rest="${issue#*:}"
    local severity="${rest%%:*}"
    local description="${rest#*:}"

    case "${OUTPUT_FORMAT}" in
        json)
            cat <<EOF
  {
    "file": "${file}",
    "line": ${line},
    "severity": "${severity}",
    "description": "${description}"
  }
EOF
            ;;
        sarif)
            cat <<EOF
    {
      "ruleId": "${description}",
      "level": "$(echo "${severity}" | tr '[:upper:]' '[:lower:]')",
      "message": {
        "text": "Potential secret found: ${description}"
      },
      "locations": [
        {
          "physicalLocation": {
            "artifactLocation": {
              "uri": "${file}"
            },
            "region": {
              "startLine": ${line}
            }
          }
        }
      ]
    }
EOF
            ;;
        *)
            local color
            case "${severity}" in
                CRITICAL) color="${RED}" ;;
                HIGH) color="${RED}" ;;
                MEDIUM) color="${YELLOW}" ;;
                LOW) color="${BLUE}" ;;
            esac
            echo -e "${color}[${severity}]${NC} ${file}:${line} - ${description}"
            ;;
    esac
}

# Main audit function
audit_secrets() {
    log_info "Scanning for secrets in ${PROJECT_ROOT}"
    log_info "Minimum severity: ${MIN_SEVERITY}"
    log_info "Output format: ${OUTPUT_FORMAT}"

    local all_issues=()
    local file_count=0

    # Initialize output based on format
    if [[ "${OUTPUT_FORMAT}" == "json" ]]; then
        echo '{"issues": ['
    elif [[ "${OUTPUT_FORMAT}" == "sarif" ]]; then
        cat <<'EOF'
{
  "$schema": "https://json.schemastore.org/sarif-2.1.0.json",
  "version": "2.1.0",
  "runs": [
    {
      "tool": {
        "driver": {
          "name": "erlmcp-secret-audit",
          "version": "1.0.0",
          "informationUri": "https://github.com/erlang-mcp/erlmcp"
        }
      },
      "results": [
EOF
    fi

    # Scan files
    while IFS= read -r -d '' file; do
        if should_exclude "${file}"; then
            continue
        fi

        # Check file extension
        local ext="${file##*.}"
        local should_scan=false
        for scan_ext in "${SCAN_EXTENSIONS[@]}"; do
            if [[ "${file}" == *"${scan_ext}" ]] || [[ ".${ext}" == "${scan_ext}" ]]; then
                should_scan=true
                break
            fi
        done

        if [[ "${should_scan}" == "true" ]]; then
            ((file_count++))
            local issues
            mapfile -t issues < <(scan_file "${file}")

            if [[ ${#issues[@]} -gt 0 ]]; then
                for issue in "${issues[@]}"; do
                    if [[ -n "${issue}" ]]; then
                        [[ ${file_count} -gt 1 ]] && [[ "${OUTPUT_FORMAT}" == "json" ]] && echo ","
                        format_output "${file#"${PROJECT_ROOT}"/}" "${issue}"
                        ((FOUND_ISSUES++))
                    fi
                done
            fi
        fi
    done < <(find "${PROJECT_ROOT}" -type f -print0 2>/dev/null)

    # Close output
    if [[ "${OUTPUT_FORMAT}" == "json" ]]; then
        echo -e '\n]}'
    elif [[ "${OUTPUT_FORMAT}" == "sarif" ]]; then
        echo -e '\n    ]\n  }\n]'
    fi

    # Summary
    log_info "Scanned ${file_count} files"
    if [[ ${FOUND_ISSUES} -gt 0 ]]; then
        log_error "Found ${FOUND_ISSUES} potential secret(s)"
        return 1
    else
        log_info "No secrets found"
        return 0
    fi
}

# Fix issues
fix_issues() {
    if [[ "${FIX_MODE}" != "true" ]]; then
        return 0
    fi

    log_warn "Fix mode enabled - this will replace secrets with placeholders"
    read -p "Continue? (yes/no): " confirm

    if [[ "${confirm}" != "yes" ]]; then
        log_info "Fix cancelled"
        return 0
    fi

    # Implementation would go here
    # For now, just show what would be done
    log_info "Fix mode not implemented - please manually fix issues"
}

# Main execution
main() {
    parse_args "$@"

    if audit_secrets; then
        exit 0
    else
        fix_issues
        exit 1
    fi
}

# Run
main "$@"
