#!/bin/bash

#=============================================================================
# erlmcp CHANGELOG Generator
#=============================================================================
# Usage: ./tools/changelog-generator.sh [options]
#
# This script generates CHANGELOG entries from git commits using
# conventional commit format (feat:, fix:, chore:, etc.)
#
# Options:
#   --since <version>     Generate commits since version (default: previous tag)
#   --until <version>     Generate commits until version (default: HEAD)
#   --update-file         Update CHANGELOG.md with new entries
#   --dry-run             Show what would be done without making changes
#=============================================================================

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
CHANGELOG_FILE="$PROJECT_ROOT/CHANGELOG.md"

# Options
SINCE=""
UNTIL="HEAD"
UPDATE_FILE=false
DRY_RUN=false

#=============================================================================
# Functions
#=============================================================================

log_info() {
    echo -e "${BLUE}[INFO]${NC} $*" >&2
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*" >&2
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*" >&2
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*" >&2
}

# Get previous git tag
get_previous_tag() {
    local current_tag="${1:-}"

    if [ -z "$current_tag" ]; then
        # Get most recent tag
        git -C "$PROJECT_ROOT" describe --tags --abbrev=0 2>/dev/null || echo ""
    else
        # Get tag before current
        git -C "$PROJECT_ROOT" tag --sort=-version:refname | grep -v "^$current_tag$" | head -1 || echo ""
    fi
}

# Get commits between versions
get_commits() {
    local since=$1
    local until=$2
    local range

    if [ -z "$since" ]; then
        range="$until"
    else
        range="${since}..${until}"
    fi

    git -C "$PROJECT_ROOT" log "$range" --pretty=format:"%h|%s|%b" 2>/dev/null || true
}

# Parse conventional commit
parse_commit() {
    local hash=$1
    local subject=$2
    local body=$3

    # Extract type and scope from subject
    # Format: type(scope): message or type: message
    local type scope message pr_issue

    # Extract type (first word up to : or ()
    if [[ "$subject" == *":"* ]]; then
        # Has colon - extract type before colon
        type="${subject%%:*}"
        # Clean up type - remove parentheses if present
        type="${type%% *}"
        type="${type%%(*}"

        # Extract scope if present in parentheses
        if [[ "$type" == *"("* ]]; then
            scope="${type#*(}"
            scope="${scope%)*}"
            type="${type%%(*}"
        fi

        # Extract message after colon
        message="${subject#*:}"
        message="${message# }"  # Remove leading space
    fi

    # Extract PR/issue from body (simple string search)
    if [[ "$body" == *"Closes #"* ]]; then
        pr_issue=$(echo "$body" | grep -oP 'Closes #\K[0-9]+' | head -1)
        pr_issue="#$pr_issue"
    elif [[ "$body" == *"Fixes #"* ]]; then
        pr_issue=$(echo "$body" | grep -oP 'Fixes #\K[0-9]+' | head -1)
        pr_issue="#$pr_issue"
    elif [[ "$body" == *"Resolves #"* ]]; then
        pr_issue=$(echo "$body" | grep -oP 'Resolves #\K[0-9]+' | head -1)
        pr_issue="#$pr_issue"
    fi

    # Only output if we found a type and message
    if [ -n "$type" ] && [ -n "$message" ]; then
        echo "$type|$scope|$message|$hash|$pr_issue"
    fi
}

# Group commits by type
group_commits() {
    local type=$1
    local label=$2

    echo ""
    echo "### $label"
    echo ""

    while IFS='|' read -r commit_type scope message hash pr_issue; do
        if [ "$commit_type" = "$type" ]; then
            local scope_str=""
            if [ -n "$scope" ]; then
                scope_str="${CYAN}($scope)${NC} "
            fi

            local pr_str=""
            if [ -n "$pr_issue" ]; then
                pr_str=" (${GREEN}$pr_issue${NC})"
            fi

            # Format: - scope: message (link)
            if [ -n "$scope" ]; then
                echo "- **${scope}**: $message$pr_str"
            else
                echo "- $message$pr_str"
            fi
        fi
    done

    echo ""
}

# Generate changelog for version
generate_changelog_entry() {
    local version=$1
    local since=$2
    local until=$3

    log_info "Generating changelog for $version"
    log_info "Range: $since..$until"

    # Get commits
    local commits=$(get_commits "$since" "$until")

    if [ -z "$commits" ]; then
        log_warn "No commits found in range"
        return 1
    fi

    # Parse commits into array
    local parsed_commits=()
    while IFS= read -r line; do
        IFS='|' read -r hash subject body <<< "$line"
        local parsed=$(parse_commit "$hash" "$subject" "$body")
        if [ -n "$parsed" ]; then
            parsed_commits+=("$parsed")
        fi
    done <<< "$commits"

    if [ ${#parsed_commits[@]} -eq 0 ]; then
        log_warn "No conventional commits found"
        return 1
    fi

    # Generate changelog header
    local date=$(date +%Y-%m-%d)
    echo ""
    echo "## [$version] - $date"
    echo ""

    # Group by type
    local temp_file=$(mktemp)
    printf '%s\n' "${parsed_commits[@]}" > "$temp_file"

    # Breaking Changes
    if grep -q "^feat|" "$temp_file" 2>/dev/null; then
        echo "### Breaking Changes"
        echo ""
        grep "^feat|" "$temp_file" | while IFS='|' read -r type scope message hash pr_issue; do
            if [[ $message =~ !(.*) ]]; then
                local scope_str=""
                [ -n "$scope" ] && scope_str="**${scope}**: "
                echo "- ${scope_str}${message%!*}$pr_issue"
            fi
        done
        echo ""
    fi

    # New Features
    if grep -q "^feat|" "$temp_file" 2>/dev/null; then
        echo "### New Features"
        echo ""
        grep "^feat|" "$temp_file" | while IFS='|' read -r type scope message hash pr_issue; do
            if [[ ! $message =~ !(.*) ]]; then
                local scope_str=""
                [ -n "$scope" ] && scope_str="**${scope}**: "
                echo "- ${scope_str}${message}"
                [ -n "$pr_issue" ] && echo "  [$pr_issue](https://github.com/banyan-platform/erlmcp/issues/${pr_issue#\#})"
            fi
        done
        echo ""
    fi

    # Bug Fixes
    if grep -q "^fix|" "$temp_file" 2>/dev/null; then
        echo "### Bug Fixes"
        echo ""
        grep "^fix|" "$temp_file" | while IFS='|' read -r type scope message hash pr_issue; do
            local scope_str=""
            [ -n "$scope" ] && scope_str="**${scope}**: "
            echo "- ${scope_str}${message}"
            [ -n "$pr_issue" ] && echo "  [$pr_issue](https://github.com/banyan-platform/erlmcp/issues/${pr_issue#\#})"
        done
        echo ""
    fi

    # Enhancements
    if grep -q "^enhancement|" "$temp_file" 2>/dev/null; then
        echo "### Enhancements"
        echo ""
        grep "^enhancement|" "$temp_file" | while IFS='|' read -r type scope message hash pr_issue; do
            local scope_str=""
            [ -n "$scope" ] && scope_str="**${scope}**: "
            echo "- ${scope_str}${message}"
            [ -n "$pr_issue" ] && echo "  [$pr_issue](https://github.com/banyan-platform/erlmcp/issues/${pr_issue#\#})"
        done
        echo ""
    fi

    # Documentation
    if grep -q "^docs|" "$temp_file" 2>/dev/null; then
        echo "### Documentation"
        echo ""
        grep "^docs|" "$temp_file" | while IFS='|' read -r type scope message hash pr_issue; do
            local scope_str=""
            [ -n "$scope" ] && scope_str="**${scope}**: "
            echo "- ${scope_str}${message}"
        done
        echo ""
    fi

    # Internal Changes
    if grep -q "^chore\|^refactor\|^perf" "$temp_file" 2>/dev/null; then
        echo "### Internal"
        echo ""
        grep "^chore\|^refactor\|^perf" "$temp_file" | while IFS='|' read -r type scope message hash pr_issue; do
            local scope_str=""
            [ -n "$scope" ] && scope_str="**${scope}**: "
            echo "- ${scope_str}${message}"
        done
        echo ""
    fi

    rm -f "$temp_file"

    # Summary
    log_success "Changelog generated for $version"
}

# Update CHANGELOG.md file
update_changelog_file() {
    local version=$1
    local new_content=$2

    if [ ! -f "$CHANGELOG_FILE" ]; then
        log_info "Creating new CHANGELOG.md"
        echo "# Changelog" > "$CHANGELOG_FILE"
        echo "" >> "$CHANGELOG_FILE"
        echo "All notable changes to erlmcp are documented in this file." >> "$CHANGELOG_FILE"
        echo "" >> "$CHANGELOG_FILE"
        echo "The format is based on [Keep a Changelog](https://keepachangelog.com/)" >> "$CHANGELOG_FILE"
        echo "and this project adheres to [Semantic Versioning](https://semver.org/)." >> "$CHANGELOG_FILE"
        echo "" >> "$CHANGELOG_FILE"
    fi

    # Insert new content after header
    local temp_file=$(mktemp)
    {
        head -n 6 "$CHANGELOG_FILE"
        echo "$new_content"
        tail -n +7 "$CHANGELOG_FILE"
    } > "$temp_file"

    mv "$temp_file" "$CHANGELOG_FILE"
    log_success "Updated $CHANGELOG_FILE"
}

# Parse command-line arguments
parse_args() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            --since)
                SINCE="$2"
                shift 2
                ;;
            --until)
                UNTIL="$2"
                shift 2
                ;;
            --update-file)
                UPDATE_FILE=true
                shift
                ;;
            --dry-run)
                DRY_RUN=true
                shift
                ;;
            *)
                log_error "Unknown option: $1"
                return 1
                ;;
        esac
    done
}

# Display help
show_help() {
    cat << EOF
${BLUE}erlmcp CHANGELOG Generator${NC}

Usage: $0 [options]

Options:
  --since <version>     Generate commits since version
                        Default: most recent tag
  --until <version>     Generate commits until version
                        Default: HEAD
  --update-file         Update CHANGELOG.md with generated entries
  --dry-run             Show output without modifying files

Examples:
  # Generate changelog since last tag
  $0 --dry-run

  # Generate for specific version range
  $0 --since v1.1.0 --until v1.2.0 --dry-run

  # Update CHANGELOG.md with new entries
  $0 --since v1.1.0 --update-file

For more information, see RELEASE_STRATEGY.md
EOF
}

#=============================================================================
# Main
#=============================================================================

main() {
    log_info "erlmcp CHANGELOG Generator v1.0.0"

    # Parse arguments
    if ! parse_args "$@"; then
        show_help
        return 1
    fi

    # Change to project root
    cd "$PROJECT_ROOT" || return 1

    # Determine version range
    if [ -z "$SINCE" ]; then
        SINCE=$(get_previous_tag)
        if [ -z "$SINCE" ]; then
            log_warn "No previous tag found, generating for all commits"
            SINCE=""
        else
            log_info "Previous tag: $SINCE"
        fi
    fi

    # Generate changelog
    local changelog_entry
    if ! changelog_entry=$(generate_changelog_entry "UNRELEASED" "$SINCE" "$UNTIL" 2>&1); then
        log_error "Failed to generate changelog"
        return 1
    fi

    # Display output
    if [ "$DRY_RUN" = true ]; then
        log_info "Dry run - not modifying files"
        echo ""
        echo "Generated changelog:"
        echo "$changelog_entry"
        echo ""
        log_info "To apply changes, run: $0 --update-file"
    else
        if [ "$UPDATE_FILE" = true ]; then
            update_changelog_file "UNRELEASED" "$changelog_entry"
            log_success "CHANGELOG.md updated"
        else
            log_warn "Not updating file (use --update-file to apply changes)"
        fi
    fi

    return 0
}

# Show help if requested
if [ "${1:-}" = "--help" ] || [ "${1:-}" = "-h" ]; then
    show_help
    exit 0
fi

# Run main function
main "$@"
