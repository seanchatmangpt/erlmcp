#!/bin/bash

#=============================================================================
# erlmcp Release Script
#=============================================================================
# Usage: ./tools/release.sh <version>
# Example: ./tools/release.sh 1.2.0
#
# This script:
# 1. Validates version format (semantic versioning)
# 2. Updates version numbers in source files
# 3. Updates CHANGELOG
# 4. Creates git tag
# 5. Pushes to repository (triggers CI/CD)
#=============================================================================

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
VERSION="${1:-}"

#=============================================================================
# Functions
#=============================================================================

log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

# Validate version format (MAJOR.MINOR.PATCH[-PRERELEASE])
validate_version() {
    local version=$1
    local pattern='^[0-9]+\.[0-9]+\.[0-9]+(-[a-zA-Z0-9]+)?(\.[0-9]+)?$'

    if [[ ! $version =~ $pattern ]]; then
        log_error "Invalid version format: $version"
        log_error "Expected format: MAJOR.MINOR.PATCH[-PRERELEASE]"
        log_error "Examples: 1.0.0, 1.2.3-beta.1, 1.2.0-rc.1"
        return 1
    fi
    return 0
}

# Check git status is clean
check_git_status() {
    if ! git -C "$PROJECT_ROOT" diff-index --quiet HEAD --; then
        log_error "Working directory has uncommitted changes"
        log_error "Please commit or stash changes before releasing"
        return 1
    fi
    return 0
}

# Check if tag already exists
check_tag_exists() {
    local tag="v$1"
    if git -C "$PROJECT_ROOT" rev-parse "$tag" > /dev/null 2>&1; then
        log_error "Tag $tag already exists"
        return 1
    fi
    return 0
}

# Update version in rebar.config
update_rebar_config() {
    local version=$1
    local rebar_file="$PROJECT_ROOT/rebar.config"

    if [ ! -f "$rebar_file" ]; then
        log_warn "rebar.config not found, skipping"
        return 0
    fi

    # Update {vsn, "..."} in first occurrence
    sed -i.bak "/{vsn,/s/\"[^\"]*\"/\"$version\"/" "$rebar_file" && rm -f "$rebar_file.bak"
    log_info "Updated rebar.config with version $version"
}

# Update version in app file
update_app_file() {
    local version=$1
    local app_file="$PROJECT_ROOT/src/erlmcp.app.src"

    if [ ! -f "$app_file" ]; then
        log_warn "erlmcp.app.src not found, skipping"
        return 0
    fi

    # Update {vsn, "..."} in app file
    sed -i.bak "/{vsn,/s/\"[^\"]*\"/\"$version\"/" "$app_file" && rm -f "$app_file.bak"
    log_info "Updated erlmcp.app.src with version $version"
}

# Create version.erl if not exists
create_version_erl() {
    local version=$1
    local version_file="$PROJECT_ROOT/src/erlmcp_version.erl"

    if [ -f "$version_file" ]; then
        # Update existing file
        sed -i.bak "s/{erlmcp_vsn,.*}/{erlmcp_vsn, \"$version\"}/" "$version_file" && rm -f "$version_file.bak"
    else
        # Create new file
        cat > "$version_file" << 'EOF'
%% Auto-generated version file - do not edit
-module(erlmcp_version).

%% Public API
-export([version/0, major/0, minor/0, patch/0]).

%% Version as semantic versioning string
version() ->
    {erlmcp_vsn, "VERSION_PLACEHOLDER"}.

%% Major version (breaking changes)
major() ->
    element(2, string:to_integer(hd(string:split(element(2, version()), ".")))).

%% Minor version (backward-compatible features)
minor() ->
    [_|Rest] = string:split(element(2, version()), "."),
    element(2, string:to_integer(hd(Rest))).

%% Patch version (bug fixes)
patch() ->
    Tokens = string:split(element(2, version()), "."),
    Patch = lists:nth(3, Tokens),
    PatchNum = string:to_integer(hd(string:split(Patch, "-"))),
    element(2, PatchNum).
EOF
        sed -i.bak "s/VERSION_PLACEHOLDER/$version/" "$version_file" && rm -f "$version_file.bak"
    fi

    log_info "Created/updated erlmcp_version.erl with version $version"
}

# Validate that version bumps compile
validate_build() {
    log_info "Validating build after version bump..."

    if ! rebar3 -C "$PROJECT_ROOT/rebar.config" check 2>/dev/null; then
        log_error "Build validation failed after version bump"
        log_error "Please fix compilation errors before releasing"
        return 1
    fi

    log_success "Build validation passed"
    return 0
}

# Run test suite
run_tests() {
    log_info "Running test suite..."

    if ! rebar3 -C "$PROJECT_ROOT/rebar.config" eunit 2>/dev/null; then
        log_error "Unit tests failed"
        log_error "Please fix test failures before releasing"
        return 1
    fi

    log_success "All tests passed"
    return 0
}

# Create git tag
create_git_tag() {
    local version=$1
    local tag="v$version"

    # Generate tag message with changelog excerpt
    local changelog_file="$PROJECT_ROOT/CHANGELOG.md"
    local tag_message="Release $tag"

    if [ -f "$changelog_file" ]; then
        # Extract changelog for this version
        local changelog_excerpt=$(sed -n "/^## \[$version\]/,/^## \[/p" "$changelog_file" | head -n -1)
        if [ -n "$changelog_excerpt" ]; then
            tag_message="Release $tag

$(echo "$changelog_excerpt" | tail -n +2)"
        fi
    fi

    log_info "Creating git tag: $tag"
    git -C "$PROJECT_ROOT" tag -a "$tag" -m "$tag_message"
    log_success "Git tag created: $tag"
}

# Push to repository
push_to_repo() {
    local version=$1
    local tag="v$version"

    log_info "Pushing changes to repository..."

    # Push commits
    if ! git -C "$PROJECT_ROOT" push origin HEAD 2>/dev/null; then
        log_warn "Could not push commits (may already be pushed)"
    fi

    # Push tag (this triggers CI/CD)
    if ! git -C "$PROJECT_ROOT" push origin "$tag" 2>/dev/null; then
        log_error "Failed to push tag: $tag"
        log_error "Please ensure you have push permissions"
        return 1
    fi

    log_success "Pushed tag to repository: $tag"
    log_info "GitHub Actions should now build and release v$version"
    return 0
}

# Display pre-release checklist
show_checklist() {
    cat << EOF

${BLUE}Release Checklist for v$1${NC}

Before pushing, ensure you have completed:
  [ ] Updated CHANGELOG.md with release notes
  [ ] All tests passing locally
  [ ] Code review completed
  [ ] Security scan passed
  [ ] Documentation updated
  [ ] Migration guide prepared (if breaking changes)
  [ ] Team notification sent

Press Ctrl+C to cancel, or Enter to continue...

EOF
    read -r
}

# Display post-release instructions
show_post_release_instructions() {
    local version=$1
    local tag="v$version"

    cat << EOF

${GREEN}Release Process Initiated!${NC}

Next steps:
1. Monitor GitHub Actions workflow for v$version
   URL: https://github.com/banyan-platform/erlmcp/actions

2. Watch CI/CD pipeline:
   - Build & test suite
   - Docker image build
   - Deploy to staging
   - Run integration tests
   - Deploy to production (manual approval)

3. Verify release:
   - Check GitHub releases page
   - Verify Docker image availability
   - Check package manager (Hex)
   - Verify metrics and monitoring

4. Post-release:
   - Announce release on channels
   - Monitor production metrics
   - Be ready for hotfix if needed

5. If issues found:
   ./tools/release.sh 1.2.1  # Create hotfix release

Documentation:
- Release Strategy: RELEASE_STRATEGY.md
- Changelog: CHANGELOG.md
- Development: DEVELOPMENT.md

Questions? See CONTRIBUTING.md

EOF
}

#=============================================================================
# Main
#=============================================================================

main() {
    log_info "erlmcp Release Tool v1.0.0"

    # Check arguments
    if [ -z "$VERSION" ]; then
        log_error "Version number required"
        echo ""
        echo "Usage: $0 <version>"
        echo ""
        echo "Examples:"
        echo "  $0 1.0.0          # Release version 1.0.0"
        echo "  $0 1.2.0-beta.1   # Release beta version"
        echo "  $0 1.2.0-rc.1     # Release release candidate"
        return 1
    fi

    # Validate version format
    if ! validate_version "$VERSION"; then
        return 1
    fi

    log_success "Valid version format: $VERSION"

    # Change to project root
    cd "$PROJECT_ROOT" || return 1

    # Pre-flight checks
    log_info "Running pre-flight checks..."

    if ! check_git_status; then
        return 1
    fi
    log_success "Working directory clean"

    if ! check_tag_exists "$VERSION"; then
        return 1
    fi
    log_success "Tag v$VERSION does not exist"

    # Update version files
    log_info "Updating version files..."
    update_rebar_config "$VERSION"
    update_app_file "$VERSION"
    create_version_erl "$VERSION"

    # Validate changes
    log_info "Validating changes..."
    if ! validate_build; then
        return 1
    fi

    if ! run_tests; then
        return 1
    fi

    # Commit version changes
    log_info "Committing version changes..."
    git add -A
    git commit -m "chore: bump version to $VERSION" || log_warn "No changes to commit"
    log_success "Version changes committed"

    # Show checklist
    show_checklist "$VERSION"

    # Create tag and push
    if ! create_git_tag "$VERSION"; then
        return 1
    fi

    if ! push_to_repo "$VERSION"; then
        return 1
    fi

    # Display post-release instructions
    show_post_release_instructions "$VERSION"

    log_success "Release $VERSION initiated successfully!"
    return 0
}

# Run main function
main "$@"
