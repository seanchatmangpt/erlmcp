#!/usr/bin/env python3
"""
Check for duplicate export declarations in Erlang files.
"""

import re
import sys
from pathlib import Path
from typing import List, Tuple


def check_duplicate_exports(filepath: Path) -> List[Tuple[str, int]]:
    """Check for duplicate exports in a single file."""
    try:
        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read()
    except Exception:
        return []

    all_exports = []
    # Pattern to match -export([...]) blocks
    pattern = r'-export\s*\(\s*\[\s*([^\]]+(?:\[[^\]]*\][^\]]*)*)\s*\]\s*\)\s*\.'

    for match in re.finditer(pattern, content):
        export_content = match.group(1)

        for item in re.split(r',\s*', export_content.strip()):
            item = item.strip()
            if not item or item.startswith('%'):
                continue
            if '/' in item:
                # Remove trailing comments
                if '%' in item:
                    item = item[:item.index('%')].strip()
                if not item:
                    continue
                parts = item.split('/', 1)
                if len(parts) == 2:
                    name, arity_str = parts
                    name = name.strip()
                    arity_str = arity_str.strip()
                    try:
                        arity = int(arity_str)
                        all_exports.append((name, arity))
                    except ValueError:
                        pass

    # Find duplicates
    seen = set()
    duplicates = []
    for export in all_exports:
        if export in seen:
            duplicates.append(export)
        seen.add(export)

    return list(set(duplicates))


def check_behavior_callback_exports(filepath: Path) -> List[str]:
    """Check if behavior files properly export callbacks."""
    try:
        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read()
    except Exception:
        return []

    issues = []

    # Check if this is a behavior file
    is_behavior = '-callback' in content or '-behaviour(' in content or '-behavior(' in content

    if is_behavior:
        # Extract callback definitions
        callbacks = set()
        for match in re.finditer(r'-callback\s+(\w+)\s*\(([^)]*)\)', content):
            name = match.group(1)
            args = match.group(2)
            arity = 0 if not args.strip() else len([a for a in args.split(',') if a.strip()])
            callbacks.add((name, arity))

        # Extract exports
        exports = set()
        for match in re.finditer(r'-export\s*\(\s*\[\s*([^\]]+)\s*\]\s*\)', content):
            export_content = match.group(1)
            for item in re.split(r',\s*', export_content.strip()):
                item = item.strip()
                if '/' in item and not item.startswith('%'):
                    parts = item.split('/', 1)
                    if len(parts) == 2:
                        name, arity_str = parts
                        try:
                            arity = int(arity_str.strip())
                            exports.add((name, arity))
                        except ValueError:
                            pass

        # Check if callbacks are exported
        for callback in callbacks:
            if callback not in exports:
                issues.append(f"Callback {callback[0]}/{callback[1]} not exported")

    return issues


def main():
    if len(sys.argv) < 2:
        print("Usage: check_duplicates.py <directory>")
        sys.exit(1)

    directory = sys.argv[1]
    dir_path = Path(directory)

    if not dir_path.exists():
        print(f"Error: Directory not found: {directory}")
        sys.exit(1)

    erl_files = sorted([
        f for f in dir_path.rglob('*.erl')
        if '_build' not in str(f) and '_archived' not in str(f)
    ])

    print(f"Checking {len(erl_files)} files for duplicate exports and behavior issues...\n")

    duplicate_count = 0
    behavior_issue_count = 0

    for filepath in erl_files:
        duplicates = check_duplicate_exports(filepath)
        behavior_issues = check_behavior_callback_exports(filepath)

        if duplicates or behavior_issues:
            rel_path = filepath.relative_to(dir_path) if filepath.is_absolute() else filepath
            print(f"{rel_path}:")
            if duplicates:
                duplicate_count += len(duplicates)
                for name, arity in sorted(duplicates):
                    print(f"  Duplicate export: {name}/{arity}")
            if behavior_issues:
                behavior_issue_count += len(behavior_issues)
                for issue in behavior_issues:
                    print(f"  {issue}")
            print()

    print(f"SUMMARY:")
    print(f"  Files analyzed: {len(erl_files)}")
    print(f"  Duplicate exports found: {duplicate_count}")
    print(f"  Behavior callback issues: {behavior_issue_count}")

    if duplicate_count == 0 and behavior_issue_count == 0:
        print("\nNo duplicate exports or behavior issues found!")
        sys.exit(0)
    else:
        sys.exit(1)


if __name__ == '__main__':
    main()
