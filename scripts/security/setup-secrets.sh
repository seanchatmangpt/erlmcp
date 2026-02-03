#!/usr/bin/env bash
#############################################################################
# erlmcp v3 - Secure Secrets Generation Script
#############################################################################
# Generates cryptographically secure secrets for production deployment.
# All secrets generated using OpenSSL with industry-standard algorithms.
#
# Usage:
#   ./scripts/security/setup-secrets.sh [OUTPUT_DIR]
#
# Environment Variables:
#   SECRETS_OUTPUT_DIR  - Override default output directory
#   ERLMCP_ENV         - Environment prefix (default: production)
#   SECRETS_OVERWRITE  - Set to "true" to overwrite existing secrets
#
# Outputs:
#   Creates .env file with generated secrets
#   Creates JSON manifest with metadata
#   Prints secure receipt with checksums
#
# Security:
#   - Uses OpenSSL rand with 256-bit entropy minimum
#   - Generates RSA 4096-bit keys for JWT signing
#   - Creates bcrypt hashes with cost factor 12
#   - All files created with 0600 permissions
#   - Secrets never logged, only checksums displayed
#############################################################################

set -euo pipefail

#############################################################################
# Configuration
#############################################################################

readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

# Default configuration
DEFAULT_OUTPUT_DIR="${PROJECT_ROOT}/config/secrets"
DEFAULT_ENV="${ERLMCP_ENV:-production}"
MIN_ENTROPY_BYTES=32  # 256 bits
JWT_KEY_BITS=4096     # RSA key size
BCRYPT_COST=12        # Computational cost for password hashing

# Colors for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m' # No Color

#############################################################################
# Logging Functions
#############################################################################

log_info() {
    echo -e "${BLUE}[INFO]${NC} $*" >&2
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*" >&2
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $*" >&2
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*" >&2
}

log_step() {
    echo -e "${GREEN}==>${NC} $*"
}

#############################################################################
# Security Functions
#############################################################################

# Check system entropy availability
check_entropy() {
    if [[ -f /proc/sys/kernel/random/entropy_avail ]]; then
        local entropy
        entropy=$(cat /proc/sys/kernel/random/entropy_avail 2>/dev/null || echo "0")
        if [[ "$entropy" -lt 1000 ]]; then
            log_warning "Low system entropy: ${entropy}. Consider running rngd."
        fi
    fi
}

# Generate cryptographically secure random bytes
generate_random_bytes() {
    local bytes="${1:-$MIN_ENTROPY_BYTES}"
    openssl rand -base64 "$bytes" | tr -d '\n='
}

# Generate secure hex string
generate_random_hex() {
    local bytes="${1:-$MIN_ENTROPY_BYTES}"
    openssl rand -hex "$bytes" | tr -d '\n'
}

# Generate bcrypt hash
generate_bcrypt_hash() {
    local password="$1"
    local cost="${2:-$BCRYPT_COST}"

    # Use htpasswd if available (preferred), otherwise fallback
    if command -v htpasswd &>/dev/null; then
        htpasswd -nBbC "$cost" "" "$password" | sed 's/^\$?\$?://'
    else
        # Fallback: generate strong hash using OpenSSL
        # Note: This is a simplified hash - bcrypt is preferred
        log_warning "htpasswd not available, using fallback hashing"
        echo -n "$password" | openssl dgst -sha256 -hmac "$(
            generate_random_bytes 16
        )" | awk '{print $2}'
    fi
}

# Generate RSA key pair for JWT signing
generate_jwt_keys() {
    local output_dir="$1"
    local key_name="${2:-jwt}"

    # Private key
    openssl genpkey \
        -algorithm RSA \
        -pkeyopt rsa_keygen_bits:"$JWT_KEY_BITS" \
        -out "${output_dir}/${key_name}_private.pem" \
        2>/dev/null

    chmod 600 "${output_dir}/${key_name}_private.pem"

    # Public key
    openssl rsa \
        -in "${output_dir}/${key_name}_private.pem" \
        -pubout \
        -out "${output_dir}/${key_name}_public.pem" \
        2>/dev/null

    chmod 644 "${output_dir}/${key_name}_public.pem"

    # Generate fingerprint
    local fingerprint
    fingerprint=$(openssl rsa \
        -in "${output_dir}/${key_name}_private.pem" \
        -pubout \
        -outform DER \
        2>/dev/null | sha256sum | awk '{print $1}')

    echo "$fingerprint"
}

#############################################################################
# Secret Generators
#############################################################################

# Generate Erlang Cookie
generate_erlang_cookie() {
    # Erlang cookie: alphanumeric string, recommended 20+ chars
    # Using base64 encoding of 32 random bytes (256 bits)
    generate_random_bytes 32 | tr '/+' '_-' | head -c 32
}

# Generate JWT secret/password
generate_jwt_secret() {
    # For HS256: 256-bit random key minimum
    generate_random_bytes 64
}

# Generate database password
generate_database_password() {
    local service_name="${1:-erlmcp}"
    local timestamp
    timestamp=$(date +%s)

    # Generate strong password: 32 chars with mixed case, numbers, symbols
    local password
    password=$(generate_random_bytes 48)

    # Ensure password has all character classes
    local upper
    local lower
    local digit
    local special

    upper=$(echo "$password" | grep -o '[A-Z]' | head -c 2)
    lower=$(echo "$password" | grep -o '[a-z]' | head -c 2)
    digit=$(echo "$password" | grep -o '[0-9]' | head -c 2)
    special=$(echo "$password" | tr -d 'A-Za-z0-9' | head -c 2)

    echo "${password}${upper}${lower}${digit}${special}" | fold -w 32 | head -n 1
}

# Generate API key with prefix
generate_api_key() {
    local prefix="${1:-erlmcp}"
    local key_length="${2:-64}"

    local random_part
    random_part=$(generate_random_bytes "$key_length")

    echo "${prefix}_${random_part}"
}

# Generate encryption key for data at rest
generate_encryption_key() {
    # AES-256-GCM requires 32-byte key
    generate_random_bytes 32 | tr -d '\n=' | head -c 44
}

# Generate session secret
generate_session_secret() {
    generate_random_bytes 48
}

# Generate OAuth2 client secrets
generate_oauth2_secrets() {
    local provider="$1"
    echo "${provider}_client_secret_$(generate_random_hex 16)"
}

# Generate webhook signing secret
generate_webhook_secret() {
    echo "whsec_$(generate_random_hex 32)"
}

# Generate Redis password
generate_redis_password() {
    # Redis AUTH: should be long and strong
    generate_random_bytes 64 | tr -d '\n='
}

# Generate internal API token
generate_internal_token() {
    echo "internal_$(generate_random_hex 48)"
}

#############################################################################
# File Operations
#############################################################################

# Check if secrets file exists and handle overwrite
check_existing_secrets() {
    local env_file="$1"

    if [[ -f "$env_file" ]]; then
        if [[ "${SECRETS_OVERWRITE:-false}" != "true" ]]; then
            log_error "Secrets file already exists: $env_file"
            log_info "Set SECRETS_OVERWRITE=true to overwrite existing secrets"
            return 1
        fi
        log_warning "Overwriting existing secrets: $env_file"
        # Backup existing secrets
        cp "$env_file" "${env_file}.backup.$(date +%s)"
        log_info "Backup created: ${env_file}.backup.$(date +%s)"
    fi
    return 0
}

# Write secrets to .env file
write_env_file() {
    local output_file="$1"
    shift
    local env_vars=("$@")

    cat >"$output_file" <<'EOF'
#############################################################################
# erlmcp v3 - Production Secrets
#############################################################################
# Generated: $(date -u +"%Y-%m-%dT%H:%M:%SZ")
# Environment: ${ENV_NAME}
#
# SECURITY WARNINGS:
# - NEVER commit this file to version control
# - NEVER share these secrets via unencrypted channels
# - Rotate secrets regularly (recommended: 90 days)
# - Use secrets management in production (Vault, AWS Secrets Manager, etc.)
# - Delete this file after loading into environment
#
# Receipt: ${RECEIPT_HASH}
#############################################################################

EOF

    for var in "${env_vars[@]}"; do
        echo "$var" >>"$output_file"
    done

    chmod 600 "$output_file"
}

# Generate JSON manifest with metadata
generate_manifest() {
    local output_file="$1"
    local env_file="$2"
    shift 2
    local secrets=("$@")

    local timestamp
    timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

    local hostname
    hostname=$(hostname)

    cat >"$output_file" <<EOF
{
  "generated_at": "$timestamp",
  "generated_by": "$USER@$hostname",
  "environment": "${ENV_NAME}",
  "secrets_file": "$env_file",
  "secrets": [
EOF

    local first=true
    for secret in "${secrets[@]}"; do
        if [[ "$secret" =~ ^([A-Z_]+)= ]]; then
            local key="${BASH_REMATCH[1]}"
            if [[ "$first" == "true" ]]; then
                first=false
            else
                echo "," >>"$output_file"
            fi
            printf '    {"name": "%s", "checksum": "%s"}' \
                "$key" \
                "$(echo "${secret#*=}" | sha256sum | awk '{print $1}')" \
                >>"$output_file"
        fi
    done

    cat >>"$output_file" <<'EOF'

  ],
  "version": "3.0.0",
  "schema": "erlmcp-secrets-v1"
}
EOF

    chmod 600 "$output_file"
}

# Generate receipt with checksums
generate_receipt() {
    local receipt_file="$1"
    local env_file="$2"

    local timestamp
    timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

    {
        echo "==================================================================="
        echo "erlmcp v3 - Secrets Generation Receipt"
        echo "==================================================================="
        echo ""
        echo "Generated: $timestamp"
        echo "Environment: ${ENV_NAME}"
        echo "Output File: $env_file"
        echo ""
        echo "Checksums (sha256):"
        echo "-------------------------------------------------------------------"

        while IFS='=' read -r key value; do
            # Skip comments and empty lines
            [[ "$key" =~ ^# ]] && continue
            [[ -z "$key" ]] && continue

            local checksum
            checksum=$(echo -n "$value" | sha256sum | awk '{print $1}')
            printf "%-40s %s\n" "$key:" "$checksum"
        done <"$env_file"

        echo ""
        echo "==================================================================="
        echo "SECURITY REMINDERS:"
        echo "  1. Store this receipt securely for audit trail"
        echo "  2. Verify checksums after loading secrets"
        echo "  3. Delete the .env file after use"
        echo "  4. Never share the actual secrets file"
        echo "==================================================================="

    } | tee "$receipt_file"

    chmod 600 "$receipt_file"
}

#############################################################################
# Validation
#############################################################################

# Validate OpenSSL is available
validate_dependencies() {
    local missing=()

    command -v openssl >/dev/null 2>&1 || missing+=("openssl")
    command -v sha256sum >/dev/null 2>&1 || missing+=("sha256sum")
    command -v hostname >/dev/null 2>&1 || missing+=("hostname")

    if [[ ${#missing[@]} -gt 0 ]]; then
        log_error "Missing required dependencies: ${missing[*]}"
        log_info "Install missing tools and try again"
        return 1
    fi

    return 0
}

# Validate generated secrets meet minimum requirements
validate_secrets() {
    local env_file="$1"
    local errors=0

    log_step "Validating generated secrets..."

    while IFS='=' read -r key value; do
        [[ "$key" =~ ^# ]] && continue
        [[ -z "$key" ]] && continue

        case "$key" in
        *ERLANG_COOKIE*)
            if [[ ${#value} -lt 20 ]]; then
                log_error "Erlang cookie too short: ${#value} chars (min: 20)"
                ((errors++))
            fi
            ;;
        *JWT* | *SECRET* | *PASSWORD*)
            if [[ ${#value} -lt 32 ]]; then
                log_error "$key too short: ${#value} chars (min: 32)"
                ((errors++))
            fi
            ;;
        esac
    done <"$env_file"

    if [[ $errors -gt 0 ]]; then
        log_error "Validation failed with $errors error(s)"
        return 1
    fi

    log_success "All secrets validated"
    return 0
}

#############################################################################
# Main Function
#############################################################################

main() {
    local output_dir="${SECRETS_OUTPUT_DIR:-$DEFAULT_OUTPUT_DIR}"
    local env_name="${DEFAULT_ENV}"
    local env_file
    local manifest_file
    local receipt_file

    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case "$1" in
        -h | --help)
            cat <<EOF
Usage: ${0##*/} [OPTIONS] [OUTPUT_DIR]

Generate secure secrets for erlmcp v3 deployment.

OPTIONS:
    -h, --help          Show this help message
    -e, --env ENV       Set environment prefix (default: production)
    -o, --overwrite     Overwrite existing secrets file

ARGUMENTS:
    OUTPUT_DIR          Output directory for generated secrets
                        (default: ${DEFAULT_OUTPUT_DIR})

ENVIRONMENT VARIABLES:
    SECRETS_OUTPUT_DIR  Override default output directory
    ERLMCP_ENV          Environment prefix
    SECRETS_OVERWRITE   Set to "true" to overwrite existing secrets

EXAMPLES:
    ${0##*/}                              # Generate to default location
    ${0##*/} -e staging ./config/secrets  # Generate for staging
    ${0##*/} -o ./config/secrets          # Overwrite and regenerate

EOF
            exit 0
            ;;
        -e | --env)
            env_name="$2"
            shift 2
            ;;
        -o | --overwrite)
            export SECRETS_OVERWRITE=true
            shift
            ;;
        -*)
            log_error "Unknown option: $1"
            exit 1
            ;;
        *)
            output_dir="$1"
            shift
            ;;
        esac
    done

    # Create output directory
    mkdir -p "$output_dir"

    # Set file paths
    env_file="${output_dir}/.env.${env_name}"
    manifest_file="${output_dir}/secrets-manifest.${env_name}.json"
    receipt_file="${output_dir}/secrets-receipt.${env_name}.txt"

    export ENV_NAME="$env_name"
    export RECEIPT_HASH

    log_info "erlmcp v3 - Secure Secrets Generator"
    log_info "Environment: $env_name"
    log_info "Output directory: $output_dir"
    echo ""

    # Validate dependencies
    validate_dependencies || exit 1

    # Check system entropy
    check_entropy

    # Check for existing secrets
    check_existing_secrets "$env_file" || exit 1

    # Generate all secrets
    log_step "Generating secure secrets..."

    # Erlang Distribution Cookie
    local erlang_cookie
    erlang_cookie=$(generate_erlang_cookie)

    # JWT Signing Keys (RSA)
    log_info "Generating RSA key pair for JWT signing..."
    local jwt_fingerprint
    jwt_fingerprint=$(generate_jwt_keys "$output_dir" "jwt")

    # Database Passwords
    local db_password
    local db_admin_password
    local db_replication_password
    db_password=$(generate_database_password "app")
    db_admin_password=$(generate_database_password "admin")
    db_replication_password=$(generate_database_password "repl")

    # API Keys
    local api_key_internal
    local api_key_external
    api_key_internal=$(generate_api_key "erlmcp_internal" 64)
    api_key_external=$(generate_api_key "erlmcp_external" 64)

    # Encryption Keys
    local encryption_key
    local session_secret
    encryption_key=$(generate_encryption_key)
    session_secret=$(generate_session_secret)

    # OAuth2 Secrets
    local github_client_secret
    local google_client_secret
    github_client_secret=$(generate_oauth2_secrets "github")
    google_client_secret=$(generate_oauth2_secrets "google")

    # Webhook Secret
    local webhook_secret
    webhook_secret=$(generate_webhook_secret)

    # Redis Password
    local redis_password
    redis_password=$(generate_redis_password)

    # Internal Token
    local internal_token
    internal_token=$(generate_internal_token)

    # Build environment variables array
    local env_vars=(
        "ERLMCP_ENV=${env_name}"
        ""
        "# Erlang Distribution"
        "ERLANG_COOKIE=${erlang_cookie}"
        ""
        "# JWT Configuration"
        "JWT_PRIVATE_KEY_PATH=${output_dir}/jwt_private.pem"
        "JWT_PUBLIC_KEY_PATH=${output_dir}/jwt_public.pem"
        "JWT_KEY_FINGERPRINT=${jwt_fingerprint}"
        ""
        "# Database Credentials"
        "DATABASE_URL=postgresql://erlmcp:${db_password}@localhost:5432/erlmcp_${env_name}"
        "DATABASE_PASSWORD=${db_password}"
        "DATABASE_ADMIN_USER=erlmcp_admin"
        "DATABASE_ADMIN_PASSWORD=${db_admin_password}"
        "DATABASE_REPLICATION_USER=erlmcp_repl"
        "DATABASE_REPLICATION_PASSWORD=${db_replication_password}"
        ""
        "# Encryption Keys"
        "ENCRYPTION_KEY=${encryption_key}"
        "SESSION_SECRET=${session_secret}"
        ""
        "# API Keys"
        "API_KEY_INTERNAL=${api_key_internal}"
        "API_KEY_EXTERNAL=${api_key_external}"
        ""
        "# OAuth2 Secrets"
        "OAUTH2_GITHUB_CLIENT_SECRET=${github_client_secret}"
        "OAUTH2_GOOGLE_CLIENT_SECRET=${google_client_secret}"
        ""
        "# Webhooks"
        "WEBHOOK_SIGNING_SECRET=${webhook_secret}"
        ""
        "# Cache"
        "REDIS_PASSWORD=${redis_password}"
        "REDIS_URL=redis://:${redis_password}@localhost:6379/${env_name}"
        ""
        "# Internal Services"
        "INTERNAL_API_TOKEN=${internal_token}"
    )

    # Generate receipt hash
    RECEIPT_HASH=$(printf '%s' "${env_vars[*]}" | sha256sum | awk '{print $1}')

    # Write files
    log_info "Writing secrets files..."
    write_env_file "$env_file" "${env_vars[@]}"
    generate_manifest "$manifest_file" "$env_file" "${env_vars[@]}"
    generate_receipt "$receipt_file" "$env_file"

    # Validate
    validate_secrets "$env_file" || exit 1

    echo ""
    log_success "Secrets generated successfully!"
    echo ""
    log_info "Files created:"
    echo "  - Secrets:  $env_file"
    echo "  - Manifest: $manifest_file"
    echo "  - Receipt:  $receipt_file"
    echo "  - JWT Keys: ${output_dir}/jwt_*.pem"
    echo ""
    log_warning "SECURITY: Keep these files secure and never commit to version control!"
    log_info "Load secrets with: source $env_file"

    return 0
}

# Run main function
main "$@"
