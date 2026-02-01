#!/usr/bin/env bash
# tools/incremental/detect-changes.sh
# Purpose: Detect changed files using SHA256 hashing
# Usage: ./detect-changes.sh

set -euo pipefail

CACHE_DIR=".erlmcp/cache"
HASH_FILE="$CACHE_DIR/file-hashes.json"
CHANGES_FILE="$CACHE_DIR/changes.json"

echo "Detecting file changes..."

# Ensure cache directory exists
mkdir -p "$CACHE_DIR"

# Find all source files
SOURCE_FILES=$(find apps -type f \( -name "*.erl" -o -name "*.hrl" -o -name "*.app.src" \) 2>/dev/null || true)

if [[ -z "$SOURCE_FILES" ]]; then
    echo "⚠ No source files found"
    exit 0
fi

# Compute current hashes
compute_hashes() {
    if ! command -v jq &> /dev/null; then
        echo "❌ jq not found, please install jq"
        exit 1
    fi

    declare -A HASHES
    local files_json="{"

    while IFS= read -r file; do
        if [[ -f "$file" ]]; then
            # Compute hash
            if command -v sha256sum &> /dev/null; then
                hash=$(sha256sum "$file" | awk '{print $1}')
            elif command -v shasum &> /dev/null; then
                hash=$(shasum -a 256 "$file" | awk '{print $1}')
            else
                echo "❌ No SHA256 command found"
                exit 1
            fi

            # Get file size and mtime
            if [[ "$OSTYPE" == "darwin"* ]]; then
                size=$(stat -f%z "$file" 2>/dev/null)
                mtime=$(stat -f%m "$file" 2>/dev/null)
            else
                size=$(stat -c%s "$file" 2>/dev/null)
                mtime=$(stat -c%Y "$file" 2>/dev/null)
            fi

            # Build JSON entry
            if [[ "$files_json" != "{" ]]; then
                files_json="$files_json,"
            fi
            files_json="$files_json\"$file\":{\"hash\":\"sha256:$hash\",\"size\":$size,\"last_modified\":\"$(date -u -r "$mtime" +%Y-%m-%dT%H:%M:%SZ 2>/dev/null || echo 'unknown')\"}"
        fi
    done <<< "$SOURCE_FILES"

    files_json="$files_json}"

    # Output JSON
    jq -n \
        --arg version "1.0.0" \
        --arg commit "$(git rev-parse HEAD 2>/dev/null || echo 'unknown')" \
        --arg timestamp "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
        --argjson files "$files_json" \
        '{
            version: $version,
            commit: $commit,
            timestamp: $timestamp,
            files: $files
        }'
}

CURRENT_HASHES=$(compute_hashes)

# Compare with previous hashes
if [[ ! -f "$HASH_FILE" ]]; then
    echo "⚠ No previous hash file found, treating all files as changed"
    echo "$CURRENT_HASHES" > "$HASH_FILE"

    ALL_FILES=$(echo "$CURRENT_HASHES" | jq -r '.files | keys[]')
    jq -n \
        --argjson files "$(echo "$ALL_FILES" | jq -R . | jq -s .)" \
        '{
            changed: $files,
            added: [],
            removed: [],
            all_changed: true
        }' > "$CHANGES_FILE"

    TOTAL=$(echo "$ALL_FILES" | wc -l)
    echo "✓ All $TOTAL files marked as changed (initial run)"
    exit 0
fi

# Compare hashes using jq
CHANGES=$(jq -n \
    --argjson current "$CURRENT_HASHES" \
    --slurpfile previous "$HASH_FILE" \
    '
    $current.files as $curr |
    $previous[0].files as $prev |
    {
        changed: [
            $curr | to_entries[] |
            select(($prev[.key] | .hash) != .value.hash) |
            .key
        ],
        added: [
            $curr | to_entries[] |
            select($prev[.key] == null) |
            .key
        ],
        removed: [
            $prev | to_entries[] |
            select($curr[.key] == null) |
            .key
        ],
        all_changed: false
    }
    ')

echo "$CHANGES" > "$CHANGES_FILE"

# Update hash file
echo "$CURRENT_HASHES" > "$HASH_FILE"

# Report changes
CHANGED_COUNT=$(echo "$CHANGES" | jq '.changed | length')
ADDED_COUNT=$(echo "$CHANGES" | jq '.added | length')
REMOVED_COUNT=$(echo "$CHANGES" | jq '.removed | length')

echo "✓ Change detection complete"
echo "  Changed: $CHANGED_COUNT files"
echo "  Added: $ADDED_COUNT files"
echo "  Removed: $REMOVED_COUNT files"

if [[ $CHANGED_COUNT -gt 0 ]]; then
    echo ""
    echo "Changed files:"
    echo "$CHANGES" | jq -r '.changed[]' | sed 's/^/  - /'
fi

if [[ $ADDED_COUNT -gt 0 ]]; then
    echo ""
    echo "Added files:"
    echo "$CHANGES" | jq -r '.added[]' | sed 's/^/  - /'
fi

if [[ $REMOVED_COUNT -gt 0 ]]; then
    echo ""
    echo "Removed files:"
    echo "$CHANGES" | jq -r '.removed[]' | sed 's/^/  - /'
fi
