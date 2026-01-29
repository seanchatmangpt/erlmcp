#!/usr/bin/env bash
###############################################################################
# ggen Example Script for erlmcp
#
# This script demonstrates how to use ggen with the erlmcp TCPS ontology
# to generate various artifacts.
#
# Usage:
#   ./scripts/ggen_example.sh
#
# Requirements:
#   - ggen installed (brew install seanchatmangpt/ggen/ggen)
#   - erlmcp ontologies in ontology/
#   - SPARQL queries in sparql/tcps_queries/
#   - Tera templates in templates/ggen/
###############################################################################

set -e  # Exit on error
set -u  # Exit on undefined variable

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Helper functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if ggen is installed
check_ggen_installed() {
    if ! command -v ggen &> /dev/null; then
        log_error "ggen is not installed"
        echo ""
        echo "Install with:"
        echo "  brew install seanchatmangpt/ggen/ggen"
        echo ""
        echo "Or with Docker:"
        echo "  docker pull seanchatman/ggen:6.0.0"
        echo "  alias ggen='docker run --rm -v \$(pwd):/workspace seanchatman/ggen:6.0.0'"
        exit 1
    fi
    log_success "ggen is installed"
}

# Validate configuration
validate_config() {
    log_info "Validating ggen configuration..."

    if [ ! -f "ggen.toml" ]; then
        log_error "ggen.toml not found. Run this script from the erlmcp root directory."
        exit 1
    fi

    log_success "Configuration file found"
}

# Validate ontologies
validate_ontologies() {
    log_info "Validating TCPS ontologies with SHACL..."

    # Note: This would require ggen validate command
    # For now, we'll just check that files exist

    local ontology_dir="ontology"
    local shapes_file="shapes/tcps_shapes.ttl"

    if [ ! -d "$ontology_dir" ]; then
        log_error "Ontology directory not found: $ontology_dir"
        exit 1
    fi

    if [ ! -f "$shapes_file" ]; then
        log_warn "SHACL shapes file not found: $shapes_file"
    fi

    local ttl_count=$(find "$ontology_dir" -name "*.ttl" | wc -l)
    log_success "Found $ttl_count ontology files"

    # Uncomment when ggen validate is available:
    # ggen validate --shacl
}

# Generate all artifacts
generate_all() {
    log_info "Generating all TCPS artifacts with ggen..."

    # Create output directory if it doesn't exist
    mkdir -p generated

    # Uncomment when ggen is integrated:
    # ggen sync

    log_warn "ggen sync command not yet available"
    log_info "This will generate:"
    echo "  - SKU marketplace listings"
    echo "  - Quality reports"
    echo "  - Work order RDF instances"
    echo "  - Andon event records"
    echo "  - Production receipts"
    echo "  - Erlang type definitions"
    echo "  - Standard work documentation"
}

# Generate specific artifact types
generate_sku_listings() {
    log_info "Generating SKU marketplace listings..."

    # Uncomment when ggen is integrated:
    # ggen generate --rule sku_listings

    log_warn "Example output would be in: generated/marketplace/"
}

generate_quality_reports() {
    log_info "Generating quality reports..."

    # Uncomment when ggen is integrated:
    # ggen generate --rule quality_reports

    log_warn "Example output would be in: generated/reports/"
}

generate_erlang_types() {
    log_info "Generating Erlang type definitions from ontology..."

    # Uncomment when ggen is integrated:
    # ggen generate --rule erlang_types

    log_warn "Example output would be in: include/generated_types.hrl"
}

# Show example SPARQL query execution
example_sparql_query() {
    log_info "Example: Executing SPARQL query for SKU readiness..."

    local query_file="sparql/tcps_queries/sku_readiness.rq"

    if [ -f "$query_file" ]; then
        echo ""
        echo "Query file: $query_file"
        echo "---"
        head -25 "$query_file"
        echo "---"
        echo ""
        log_info "This query would be executed against the TCPS ontology"
        log_info "Results would be passed to the sku_listing.md.tera template"
    else
        log_error "Query file not found: $query_file"
    fi
}

# Show example template
example_template() {
    log_info "Example: Viewing Tera template for SKU listings..."

    local template_file="templates/ggen/sku_listing.md.tera"

    if [ -f "$template_file" ]; then
        echo ""
        echo "Template file: $template_file"
        echo "---"
        head -40 "$template_file"
        echo "---"
        echo ""
        log_info "This template renders SPARQL query results into Markdown"
    else
        log_error "Template file not found: $template_file"
    fi
}

# Show directory structure
show_structure() {
    log_info "ggen Directory Structure:"
    echo ""
    echo "erlmcp/"
    echo "├── ggen.toml                 # ggen configuration"
    echo "├── ontology/                 # RDF/Turtle ontologies (source of truth)"
    echo "│   ├── tcps_core.ttl"
    echo "│   ├── tcps_quality.ttl"
    echo "│   └── ..."
    echo "├── shapes/                   # SHACL validation shapes"
    echo "│   └── tcps_shapes.ttl"
    echo "├── sparql/                   # SPARQL queries"
    echo "│   └── tcps_queries/"
    echo "│       ├── sku_readiness.rq"
    echo "│       ├── quality_metrics.rq"
    echo "│       └── ..."
    echo "├── templates/                # Tera templates"
    echo "│   └── ggen/"
    echo "│       ├── sku_listing.md.tera"
    echo "│       ├── receipt.json.tera"
    echo "│       └── ..."
    echo "└── generated/                # Generated artifacts (gitignored)"
    echo "    ├── marketplace/"
    echo "    ├── receipts/"
    echo "    └── docs/"
    echo ""
}

# Main menu
show_menu() {
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "  ggen Examples for erlmcp TCPS"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""
    echo "Select an option:"
    echo "  1) Validate ontologies"
    echo "  2) Generate all artifacts"
    echo "  3) Generate SKU listings"
    echo "  4) Generate quality reports"
    echo "  5) Generate Erlang types"
    echo "  6) Show example SPARQL query"
    echo "  7) Show example template"
    echo "  8) Show directory structure"
    echo "  9) Exit"
    echo ""
    read -p "Enter choice [1-9]: " choice

    case $choice in
        1) validate_ontologies ;;
        2) generate_all ;;
        3) generate_sku_listings ;;
        4) generate_quality_reports ;;
        5) generate_erlang_types ;;
        6) example_sparql_query ;;
        7) example_template ;;
        8) show_structure ;;
        9) log_info "Exiting..."; exit 0 ;;
        *) log_error "Invalid choice"; show_menu ;;
    esac

    echo ""
    read -p "Press Enter to continue..."
    show_menu
}

# Main execution
main() {
    log_info "ggen Example Script for erlmcp"
    echo ""

    check_ggen_installed
    validate_config
    show_structure

    # Interactive mode
    show_menu
}

# Run if executed directly
if [ "${BASH_SOURCE[0]}" -ef "$0" ]; then
    main "$@"
fi
