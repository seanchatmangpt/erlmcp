#!/bin/bash
# erlmcp Startup Script with Secret Manager Integration
# This script fetches secrets from Google Cloud Secret Manager and starts erlmcp

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_debug() {
    if [[ "${DEBUG:-}" == "true" ]]; then
        echo -e "${BLUE}[DEBUG]${NC} $1"
    fi
}

# Configuration
PROJECT_ID=$(gcloud config get-value project)
INSTANCE_ID=$(curl -s -H "Metadata-Flavor: Google" \
    "http://metadata.google.internal/computeMetadata/v1/instance/id")
REGION=$(curl -s -H "Metadata-Flavor: Google" \
    "http://metadata.google.internal/computeMetadata/v1/instance/region" | cut -d'/' -f4)

# Secrets configuration
declare -A SECRETS=(
    ["erlang-cookie"]="erlmcp-erlang-cookie"
    ["database-url"]="erlmcp-database-url"
    ["jwt-secret"]="erlmcp-jwt-secret"
    ["tls-cert"]="erlmcp-tls-cert"
    ["tls-key"]="erlmcp-tls-key"
    ["ca-bundle"]="erlmcp-ca-bundle"
    ["otel-ca-cert"]="erlmcp-otel-ca-cert"
)

# Fetch secret from Secret Manager
fetch_secret() {
    local secret_name="$1"
    local local_file="/tmp/${secret_name}"

    log_debug "Fetching secret: $secret_name"

    # Fetch the latest version of the secret
    gcloud secrets versions access latest \
        --secret="${secret_name}" \
        --project="${PROJECT_ID}" \
        --format='payload' | base64 -d > "${local_file}"

    chmod 600 "${local_file}"
    log_info "Secret ${secret_name} fetched to ${local_file}"
    echo "${local_file}"
}

# Check if running on GCE
check_gce() {
    if ! curl -s -H "Metadata-Flavor: Google" \
       "http://metadata.google.internal/computeMetadata/v1/" &>/dev/null; then
        log_error "Not running on Google Compute Engine"
        exit 1
    fi
}

# Verify all secrets exist
verify_secrets() {
    log_info "Verifying required secrets..."

    for secret_name in "${!SECRETS[@]}"; do
        local secret_id="${SECRETS[$secret_name]}"

        if ! gcloud secrets describe "${secret_id}" --project="${PROJECT_ID}" &>/dev/null; then
            log_error "Secret not found: ${secret_id}"
            log_info "Please create it first with:"
            log_info "  gcloud secrets create ${secret_id} --data-file=-"
            exit 1
        fi
    done

    log_info "All secrets verified"
}

# Fetch all secrets
fetch_all_secrets() {
    log_info "Fetching secrets from Secret Manager..."

    declare -g SECRET_FILES

    for secret_name in "${!SECRETS[@]}"; do
        local secret_file
        secret_file=$(fetch_secret "${SECRETS[$secret_name]}")
        SECRET_FILES["${secret_name}"]="${secret_file}"
    done

    log_info "All secrets fetched successfully"
}

# Generate temporary config files
generate_config_files() {
    log_info "Generating configuration files..."

    # Create temporary directory
    local temp_dir="/tmp/erlmcp-config"
    mkdir -p "${temp_dir}"

    # Generate sys.config
    cat > "${temp_dir}/sys.config" <<EOF
[
    {erlmcp, [
        % Basic configuration
        {cookie, "$(cat "${SECRET_FILES['erlang-cookie']}")"},
        {node_name, "erlmcp@${INSTANCE_ID}"},
        {epmd_port, 9100},
        {distribution_ports, [9100, 9101, 9102]},
        {proto_dist, inet_tls},

        % Database configuration
        {database_url, "$(cat "${SECRET_FILES['database-url']}")},

        % Security configuration
        {jwt_secret, "$(cat "${SECRET_FILES['jwt-secret']}")},
        {tls_cert_file, "${temp_dir}/server.crt"},
        {tls_key_file, "${temp_dir}/server.key"},
        {ca_bundle_file, "${temp_dir}/ca-bundle.crt"},

        % Observability
        {otel_endpoint, "https://cloud-operations.googleapis.com/v3/projects/${PROJECT_ID}/timeSeries:write"},
        {otel_ca_cert_file, "${temp_dir}/otel-ca.crt"},

        % Ports
        {http_port, 8080},
        {health_port, 9090},
        {metrics_port, 9100},

        % Logging
        {log_level, info},
        {log_dir, "/var/log/erlmcp"},
        {log_format, json}
    ]}
].
EOF

    # Copy certificate files
    cp "${SECRET_FILES['tls-cert']}" "${temp_dir}/server.crt"
    cp "${SECRET_FILES['tls-key']}" "${temp_dir}/server.key"
    cp "${SECRET_FILES['ca-bundle']}" "${temp_dir}/ca-bundle.crt"
    cp "${SECRET_FILES['otel-ca-cert']}" "${temp_dir}/otel-ca.crt"

    chmod 600 "${temp_dir}"/*.pem "${temp_dir}"/*.crt
    SECRET_FILES["config_dir"]="${temp_dir}"

    log_info "Configuration files generated"
}

# Verify Docker is running
verify_docker() {
    if ! command -v docker &> /dev/null; then
        log_error "Docker is not installed"
        exit 1
    fi

    if ! docker info &> /dev/null; then
        log_error "Docker daemon is not running"
        exit 1
    fi

    log_info "Docker verified"
}

# Pull erlmcp image
pull_erlmcp_image() {
    local image_name="${PROJECT_ID}/us-central1-docker.pkg.dev/erlmcp-marketplace/erlmcp-marketplace:latest"

    log_info "Pulling erlmcp image: ${image_name}"

    if ! docker pull "${image_name}"; then
        log_error "Failed to pull erlmcp image"
        exit 1
    fi

    log_info "erlmcp image pulled successfully"
}

# Start erlmcp container
start_erlmcp() {
    local config_dir="${SECRET_FILES['config_dir']}"

    log_info "Starting erlmcp container..."

    docker run -d \
        --name erlmcp \
        --restart unless-stopped \
        --network host \
        --volume "${config_dir}:/etc/erlmcp" \
        --volume "/var/log/erlmcp:/var/log/erlmcp" \
        --volume "/var/run/docker.sock:/var/run/docker.sock" \
        --env GOOGLE_CLOUD_PROJECT="${PROJECT_ID}" \
        --env GOOGLE_CLOUD_REGION="${REGION}" \
        --label "service=erlmcp" \
        --label "instance=${INSTANCE_ID}" \
        "${PROJECT_ID}/us-central1-docker.pkg.dev/erlmcp-marketplace/erlmcp-marketplace:latest"

    log_info "erlmcp container started successfully"
}

# Wait for erlmcp to be healthy
wait_for_health() {
    log_info "Waiting for erlmcp to be healthy..."

    local max_attempts=60
    local attempt=1

    while [ $attempt -le $max_attempts ]; do
        if curl -s "http://localhost:9090/health" | grep -q '"status":"ok"'; then
            log_info "erlmcp is healthy"
            return 0
        fi

        log_debug "Attempt $attempt: erlmcp not yet healthy..."
        sleep 5
        attempt=$((attempt + 1))
    done

    log_error "erlmcp did not become healthy after ${max_attempts} attempts"
    return 1
}

# Clean up temporary files
cleanup() {
    local config_dir="${SECRET_FILES['config_dir']:-}"

    if [[ -n "${config_dir}" && "${config_dir}" != "/" ]]; then
        log_info "Cleaning up temporary files..."
        rm -rf "${config_dir}"
    fi
}

# Main execution
main() {
    log_info "Starting erlmcp on instance ${INSTANCE_ID}..."

    check_gce
    verify_secrets
    fetch_all_secrets
    generate_config_files
    verify_docker
    pull_erlmcp_image
    start_erlmcp
    wait_for_health
    cleanup

    log_info "erlmcp startup completed successfully!"

    # Show status
    echo
    echo "=== erlmcp Status ==="
    echo "Instance ID: ${INSTANCE_ID}"
    echo "Project: ${PROJECT_ID}"
    echo "Region: ${REGION}"
    echo
    echo "Health Check: http://localhost:9090/health"
    echo "Metrics: http://localhost:9100/metrics"
    echo "API: http://localhost:8080"
    echo
    echo "Logs: /var/log/erlmcp/*.log"
    echo
    echo "To view logs:"
    echo "  docker logs erlmcp"
    echo "  journalctl -u erlmcp -f"
}

# Cleanup on exit
trap cleanup EXIT

# Run main function
main "$@"