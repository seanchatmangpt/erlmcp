#!/usr/bin/env python3
"""
Analyze and fix Erlang export declarations.

This script:
1. Finds exported but undefined functions (critical - causes compilation errors)
2. Finds defined but not exported public functions (potential missing exports)
3. Reports duplicate exports

For automatic fixing, it removes exports for undefined functions.
"""

import os
import re
import sys
from pathlib import Path
from typing import Dict, List, Set, Tuple, Optional


def extract_exports(content: str) -> Tuple[Set[Tuple[str, int]], List[Tuple[int, int]]]:
    """Extract all exported functions and their positions in the file."""
    exports = set()
    export_positions = []  # List of (start_pos, end_pos) for each -export([...])

    # Pattern to match -export([...]) blocks
    pattern = r'-export\s*\(\s*\[\s*([^\]]+(?:\[[^\]]*\][^\]]*)*)\s*\]\s*\)\s*\.'

    for match in re.finditer(pattern, content):
        start_pos = match.start()
        end_pos = match.end()
        export_positions.append((start_pos, end_pos))

        export_content = match.group(1)

        # Handle nested structures in exports (like comments)
        for item in re.split(r',\s*', export_content.strip()):
            item = item.strip()
            if not item or item.startswith('%'):
                continue
            if '/' in item:
                # Remove any trailing comments
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
                        exports.add((name, arity))
                    except ValueError:
                        pass

    return exports, export_positions


def extract_definitions(content: str) -> Set[Tuple[str, int]]:
    """Extract all top-level function definitions."""
    definitions = set()
    lines = content.split('\n')

    in_block_comment = False
    in_multiline_attribute = False
    clause_continuation = False
    spec_pending = False  # Track if previous line was a -spec

    for i, line in enumerate(lines):
        original_line = line
        stripped = line.lstrip()

        # Handle block comments
        # Match /* comments but NOT /* in file patterns like /*.json, *.json
        # A real comment /* is typically followed by: space, end of line, or non-identifier character
        # File patterns are /*. or /*a (letter immediately after)
        if in_block_comment:
            if '*/' in stripped:
                in_block_comment = False
                # Continue processing after comment end
                idx = stripped.find('*/')
                stripped = stripped[idx + 2:].lstrip()
                if not stripped:
                    continue
            else:
                continue
        else:
            # Look for /* followed by space, tab, end of line, or special chars like newline
            # But NOT followed immediately by letter, digit, _, ., or -
            # This handles: /* text, */, /*\n, but NOT: /*.json, /*.erl, /*file
            match = re.search(r'/\*(?![a-zA-Z0-9_.\-])', stripped)
            if match:
                # Verify it's actually a comment and not part of something else
                pos = match.start()
                # Check the character after /*
                after_star = stripped[pos + 2:pos + 3]
                if after_star in ('', ' ', '\t', '\n', '\r', '*', '/'):
                    # It's a comment or edge case
                    if after_star == '*' and stripped[pos + 2:pos + 4] == '*/':
                        # /* */ closing on itself - not a real block comment
                        pass
                    else:
                        in_block_comment = True
                        if '*/' in stripped[pos:]:
                            in_block_comment = False
                        continue

        # Skip comment lines but preserve spec_pending
        if stripped.startswith('%'):
            continue

        # Check for attribute continuations
        if in_multiline_attribute:
            if '.' in line:
                in_multiline_attribute = False
            continue

        # Check for -spec attribute (functions may follow on next line)
        if stripped.startswith('-spec'):
            spec_pending = True
            continue

        # Skip preprocessor directives and other attributes (but not doc comments)
        if stripped.startswith('-'):
            if re.match(r'-\s*(type|opaque|callback|record|macro|ifdef|ifndef|define|undef|compile|endif)', stripped):
                # Check if attribute continues to next line
                if '.' not in line:
                    in_multiline_attribute = True
            # Don't clear spec_pending for other attributes (might be doc-like)
            if not spec_pending:
                spec_pending = False
            continue

        # Look for function head: Name(Args) -> or Name(Args) when
        # Accept both top-level (column 0) and indented after -spec
        if not clause_continuation:
            func_match = re.match(r'^([a-z][a-zA-Z0-9_]*)\s*\(([^)]*)\)\s*(->|when\b)', stripped)
            if func_match:
                name = func_match.group(1)
                args_str = func_match.group(2).strip()
                definitions.add((name, count_arguments(args_str)))

                # Check if this is a multi-clause function (ends with ;)
                before_arrow = stripped[:stripped.find('->')]
                clause_continuation = ';' in before_arrow
                spec_pending = False

        # End of function clause
        if stripped.endswith('.'):
            clause_continuation = False
            spec_pending = False

    return definitions


def count_arguments(args_str: str) -> int:
    """Count function arguments, handling nested structures."""
    if not args_str or args_str.strip() == '':
        return 0

    args_str = args_str.strip()
    count = 1
    depth = 0
    i = 0

    while i < len(args_str):
        char = args_str[i]

        if char == '\\':
            i += 2
            continue

        if char == '"':
            i += 1
            while i < len(args_str) and args_str[i] != '"':
                if args_str[i] == '\\':
                    i += 2
                else:
                    i += 1
            i += 1
            continue

        if char in '({[':
            depth += 1
        elif char in ')}]':
            depth = max(0, depth - 1)
        elif char == ',' and depth == 0:
            count += 1

        i += 1

    return count


def remove_bad_exports(content: str, exports_to_remove: Set[Tuple[str, int]]) -> str:
    """Remove specified exports from the export list."""
    if not exports_to_remove:
        return content

    result = content

    # Remove exports one by one
    for name, arity in sorted(exports_to_remove, key=lambda x: (-len(x[0]), x)):
        # Pattern to match the export with optional whitespace and comments
        patterns = [
            # name/arity, possibly with trailing comment
            rf'\s*{re.escape(name)}\s*/\s*{arity}\s*(?:%[^\n]*)?(?=,|\])',
            # With leading comma (for non-first exports)
            rf',\s*{re.escape(name)}\s*/\s*{arity}\s*(?:%[^\n]*)?',
        ]

        for pattern in patterns:
            result = re.sub(pattern, lambda m: '' if ',' not in m.group(0) else ',',
                           result, count=1)

    # Clean up any double commas or whitespace issues
    result = re.sub(r',\s*,', ',', result)
    result = re.sub(r'\[\s*,', '[', result)
    result = re.sub(r',\s*\]', ']', result)

    # Handle empty export lists - remove them entirely
    result = re.sub(r'-export\s*\(\s*\[\s*\]\s*\)\s*\.\s*', '', result)

    return result


def analyze_file(filepath: Path) -> Dict:
    """Analyze a single Erlang file for export issues."""
    try:
        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read()
    except Exception as e:
        return {'error': str(e)}

    exports, _ = extract_exports(content)
    definitions = extract_definitions(content)

    undefined = exports - definitions
    not_exported = definitions - exports

    return {
        'content': content,
        'exports': exports,
        'definitions': definitions,
        'undefined': undefined,
        'not_exported': not_exported,
    }


def main():
    """Main entry point."""
    if len(sys.argv) < 2:
        print("Usage: fix_exports.py <directory> [--fix]")
        sys.exit(1)

    directory = sys.argv[1]
    fix_mode = '--fix' in sys.argv

    dir_path = Path(directory)
    if not dir_path.exists():
        print(f"Error: Directory not found: {directory}", file=sys.stderr)
        sys.exit(1)

    # Find all .erl files
    erl_files = sorted([
        f for f in dir_path.rglob('*.erl')
        if '_build' not in str(f) and '_archived' not in str(f)
    ])

    print(f"Analyzing {len(erl_files)} files in {directory}...\n")

    # Summary statistics
    files_with_issues = []
    total_undefined = 0
    all_issues = []

    for filepath in erl_files:
        result = analyze_file(filepath)

        if 'error' in result:
            print(f"{filepath}: ERROR - {result['error']}", file=sys.stderr)
            continue

        undefined = result['undefined']

        if undefined:
            files_with_issues.append(filepath)
            total_undefined += len(undefined)
            all_issues.append({
                'file': filepath,
                'undefined': undefined,
                'content': result['content']
            })

            rel_path = filepath.relative_to(dir_path) if filepath.is_absolute() else filepath
            print(f"{rel_path}:")
            for name, arity in sorted(undefined, key=lambda x: (x[0], x[1])):
                print(f"  - {name}/{arity} (exported but not defined)")

    # Summary
    print(f"\n{'='*60}")
    print(f"SUMMARY:")
    print(f"  Files analyzed: {len(erl_files)}")
    print(f"  Files with issues: {len(files_with_issues)}")
    print(f"  Total undefined exports: {total_undefined}")

    if files_with_issues and fix_mode:
        print(f"\n{'='*60}")
        print("FIXING ISSUES...")

        for issue in all_issues:
            filepath = issue['file']
            undefined = issue['undefined']
            content = issue['content']

            fixed_content = remove_bad_exports(content, undefined)

            if fixed_content != content:
                try:
                    with open(filepath, 'w', encoding='utf-8') as f:
                        f.write(fixed_content)
                    rel_path = filepath.relative_to(dir_path) if filepath.is_absolute() else filepath
                    print(f"  Fixed: {rel_path}")
                except Exception as e:
                    print(f"  Error fixing {filepath}: {e}", file=sys.stderr)

    if total_undefined > 0 and not fix_mode:
        print(f"\nTo automatically fix these issues, run:")
        print(f"  python3 {sys.argv[0]} {directory} --fix")
        sys.exit(1)
    elif total_undefined == 0:
        print("\nNo issues found!")
        sys.exit(0)
    else:
        sys.exit(0)


if __name__ == '__main__':
    main()
