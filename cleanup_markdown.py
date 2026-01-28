#!/usr/bin/env python3
"""
Markdown cleanup script for erlmcp repository.
Fixes common formatting issues:
- Removes trailing whitespace
- Normalizes multiple blank lines to single blank lines
- Ensures files end with a single newline
"""

import os
import sys
import re
from pathlib import Path


def cleanup_markdown_content(content):
    """Clean up markdown content."""
    # Split into lines
    lines = content.splitlines()

    # Remove trailing whitespace from each line
    lines = [line.rstrip() for line in lines]

    # Join back together
    content = '\n'.join(lines)

    # Replace 3+ consecutive newlines with 2 newlines (1 blank line)
    content = re.sub(r'\n{3,}', '\n\n', content)

    # Ensure file ends with single newline
    content = content.rstrip('\n') + '\n'

    return content


def cleanup_markdown_file(filepath):
    """Clean up a single markdown file."""
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            original_content = f.read()

        cleaned_content = cleanup_markdown_content(original_content)

        # Only write if content changed
        if cleaned_content != original_content:
            with open(filepath, 'w', encoding='utf-8') as f:
                f.write(cleaned_content)
            return True
        return False
    except Exception as e:
        print(f"Error processing {filepath}: {e}", file=sys.stderr)
        return False


def find_markdown_files(root_dir):
    """Find all markdown files in the directory tree."""
    root = Path(root_dir)
    return list(root.rglob('*.md'))


def main():
    """Main entry point."""
    root_dir = sys.argv[1] if len(sys.argv) > 1 else '.'

    print(f"Finding markdown files in {root_dir}...")
    md_files = find_markdown_files(root_dir)
    print(f"Found {len(md_files)} markdown files")

    modified_count = 0
    for filepath in md_files:
        if cleanup_markdown_file(filepath):
            modified_count += 1
            print(f"✓ Cleaned: {filepath}")

    print(f"\nCompleted: {modified_count} files modified, {len(md_files) - modified_count} files unchanged")


if __name__ == '__main__':
    main()
