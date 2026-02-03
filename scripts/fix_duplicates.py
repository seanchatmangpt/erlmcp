#!/usr/bin/env python3
"""
Fix duplicate export declarations in Erlang files.
"""

import re
import sys
from pathlib import Path
from typing import Dict, List, Set, Tuple


def find_duplicate_exports(filepath: Path) -> Tuple[List[Tuple[str, int]], str]:
    """Find duplicate exports and return original content."""
    try:
        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read()
    except Exception as e:
        return [], str(e)

    all_exports = []
    export_positions = []  # (start, end, exports_list)

    # Pattern to match -export([...]) blocks
    pattern = r'-export\s*\(\s*\[\s*([^\]]+(?:\[[^\]]*\][^\]]*)*)\s*\]\s*\)\s*\.'

    for match in re.finditer(pattern, content):
        start = match.start()
        end = match.end()
        export_content = match.group(1)

        exports = []
        for item in re.split(r',\s*', export_content.strip()):
            item = item.strip()
            if not item or item.startswith('%'):
                continue
            if '/' in item:
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
                        exports.append((name, arity, item))
                        all_exports.append((name, arity))
                    except ValueError:
                        pass

        export_positions.append((start, end, exports))

    # Find duplicates
    seen = set()
    duplicates = []
    for export in all_exports:
        if export in seen:
            duplicates.append(export)
        seen.add(export)

    return list(set(duplicates)), content


def remove_duplicate_from_export(content: str, start: int, end: int, exports: List[Tuple[str, str, str]], duplicates: Set[Tuple[str, int]]) -> str:
    """Remove duplicate exports from a specific export block."""
    export_block = content[start:end]

    # Remove duplicates (remove later occurrences)
    seen = set()
    new_exports = []
    for name, arity, original in exports:
        key = (name, arity)
        if key in seen:
            continue  # Skip duplicate
        if key in duplicates:
            seen.add(key)
            new_exports.append(original)
        else:
            seen.add(key)
            new_exports.append(original)

    # Rebuild the export block
    new_content = ', '.join(new_exports)
    new_export = f"-export([{new_content}])."

    return content[:start] + new_export + content[end:]


def fix_duplicate_exports(filepath: Path) -> bool:
    """Fix duplicate exports in a file."""
    duplicates, content_or_error = find_duplicate_exports(filepath)

    if not duplicates or isinstance(content_or_error, str):
        return False

    content = content_or_error

    # Find all export blocks
    pattern = r'-export\s*\(\s*\[\s*([^\]]+(?:\[[^\]]*\][^\]]*)*)\s*\]\s*\)\s*\.'
    duplicates_set = set(duplicates)

    modified = False
    offset = 0

    for match in re.finditer(pattern, content):
        start = match.start() + offset
        end = match.end() + offset
        export_content = match.group(1)

        exports = []
        for item in re.split(r',\s*', export_content.strip()):
            item = item.strip()
            if not item or item.startswith('%'):
                continue
            if '/' in item:
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
                        exports.append((name, arity, item))
                    except ValueError:
                        pass

        # Check for duplicates in this block
        seen = set()
        block_has_duplicates = False
        for name, arity, _ in exports:
            key = (name, arity)
            if key in seen:
                block_has_duplicates = True
                break
            seen.add(key)

        if block_has_duplicates:
            # Rebuild export without duplicates
            seen = set()
            new_exports = []
            for name, arity, original in exports:
                key = (name, arity)
                if key not in seen:
                    seen.add(key)
                    new_exports.append(original)

            new_content = ', '.join(new_exports)
            new_export = f"-export([{new_content}])."

            content = content[:start] + new_export + content[end:]
            offset += len(new_export) - (end - start)
            modified = True

    if modified:
        try:
            with open(filepath, 'w', encoding='utf-8') as f:
                f.write(content)
            return True
        except Exception:
            return False

    return False


def main():
    if len(sys.argv) < 2:
        print("Usage: fix_duplicates.py <directory> [--fix]")
        sys.exit(1)

    directory = sys.argv[1]
    fix_mode = '--fix' in sys.argv

    dir_path = Path(directory)
    if not dir_path.exists():
        print(f"Error: Directory not found: {directory}")
        sys.exit(1)

    erl_files = sorted([
        f for f in dir_path.rglob('*.erl')
        if '_build' not in str(f) and '_archived' not in str(f)
    ])

    print(f"Checking {len(erl_files)} files for duplicate exports...\n")

    files_with_duplicates = []

    for filepath in erl_files:
        duplicates, _ = find_duplicate_exports(filepath)
        if duplicates:
            files_with_duplicates.append((filepath, duplicates))

    if not files_with_duplicates:
        print("No duplicate exports found!")
        sys.exit(0)

    print(f"Found duplicate exports in {len(files_with_duplicates)} files:\n")

    for filepath, duplicates in files_with_duplicates:
        rel_path = filepath.relative_to(dir_path) if filepath.is_absolute() else filepath
        print(f"{rel_path}:")
        for name, arity in sorted(duplicates):
            print(f"  Duplicate: {name}/{arity}")

    if fix_mode:
        print(f"\nFixing duplicates...")
        fixed_count = 0
        for filepath, _ in files_with_duplicates:
            if fix_duplicate_exports(filepath):
                rel_path = filepath.relative_to(dir_path) if filepath.is_absolute() else filepath
                print(f"  Fixed: {rel_path}")
                fixed_count += 1

        print(f"\nFixed {fixed_count} files.")
    else:
        print(f"\nTo fix these issues, run:")
        print(f"  python3 {sys.argv[0]} {directory} --fix")
        sys.exit(1)


if __name__ == '__main__':
    main()
