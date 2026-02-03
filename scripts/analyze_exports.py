#!/usr/bin/env python3
"""
Analyze Erlang export declarations.

Finds:
1. Exported but undefined functions
2. Defined but not exported functions (potential missing exports)
3. Duplicate exports
"""

import os
import re
import sys
from collections import defaultdict
from pathlib import Path
from typing import Dict, List, Set, Tuple


def extract_exports(content: str) -> Set[Tuple[str, int]]:
    """Extract all exported functions from -export([...]) declarations."""
    exports = set()
    # Match -export([...]) where [...] contains function/arity pairs
    pattern = r'-export\s*\(\s*\[\s*([^\]]+)\s*\]\s*\)\s*\.'
    matches = re.findall(pattern, content)
    for match in matches:
        # Split by comma and parse each function/arity
        for item in re.split(r',\s*', match.strip()):
            item = item.strip()
            if not item:
                continue
            if '/' in item:
                name, arity = item.split('/', 1)
                try:
                    arity = int(arity)
                    exports.add((name, arity))
                except ValueError:
                    pass
    return exports


def extract_definitions(content: str) -> Set[Tuple[str, int]]:
    """Extract all function definitions from the module."""
    definitions = set()
    lines = content.split('\n')

    in_multiline_comment = False
    in_string = False
    in_attribute = False
    paren_depth = 0
    spec_depth = 0
    clause_continuation = False

    for line in lines:
        stripped = line.strip()

        # Handle multiline comments
        if '"""' in line or '''''' in line:
            pass  # Skip for now, simple handling
        if '/*' in line:
            in_multiline_comment = True
            comment_end = line.find('*/')
            if comment_end > line.find('/*'):
                in_multiline_comment = False
            continue
        if '*/' in line:
            in_multiline_comment = False
            continue

        # Skip comments and directives
        if in_multiline_comment or stripped.startswith('%') or not stripped:
            continue

        # Check for -export, -spec, -type, -record, etc. (skip these)
        if stripped.startswith('-'):
            # Track if we're in a multi-line spec/type
            if re.match(r'-\s*(spec|type|opaque|callback|record|macro)', stripped):
                in_attribute = True
            elif in_attribute and '.' in stripped:
                in_attribute = False
            continue

        if in_attribute:
            if '.' in stripped:
                in_attribute = False
            continue

        # Look for function definitions: Name(Args) -> or Name(Args) when
        # Must be at the start of a logical line (not indented clause)
        if not clause_continuation:
            # Match function definition pattern
            # Should start with lowercase letter, followed by optional alphanumeric,
            # then opening paren, arguments, closing paren, then -> or when
            func_match = re.match(
                r'^([a-z][a-zA-Z0-9_]*)\s*\(([^)]*)\)\s*(->|when\b)',
                stripped
            )
            if func_match:
                name = func_match.group(1)
                args_str = func_match.group(2).strip()

                # Calculate arity by counting arguments
                if not args_str:
                    arity = 0
                else:
                    arity = count_arguments(args_str)

                definitions.add((name, arity))

                # Check if this is a multi-clause function
                if ';' in stripped.split('->')[0]:
                    clause_continuation = True

        # Reset clause continuation if we see a . at end
        if stripped.endswith('.'):
            clause_continuation = False

    return definitions


def count_arguments(args_str: str) -> int:
    """Count function arguments, handling nested structures."""
    if not args_str or args_str.strip() == '':
        return 0

    args_str = args_str.strip()
    count = 1
    depth = 0
    in_string = False
    escape_next = False

    for char in args_str:
        if escape_next:
            escape_next = False
            continue

        if char == '\\':
            escape_next = True
            continue

        if char == '"' and not escape_next:
            in_string = not in_string
            continue

        if in_string:
            continue

        if char in '({[':
            depth += 1
        elif char in ')}]':
            depth -= 1
        elif char == ',' and depth == 0:
            count += 1

    return count


def find_duplicates(items: Set[Tuple[str, int]]) -> List[Tuple[str, int]]:
    """Find duplicate entries in the export list."""
    counts = defaultdict(int)
    for item in items:
        counts[item] += 1
    return [item for item, count in counts.items() if count > 1]


def is_public_function(name: str, arity: int) -> bool:
    """Check if a function should be in the public interface."""
    name_str = name if isinstance(name, str) else name

    # Skip test functions
    if name_str.startswith('test_'):
        return False
    if name_str.endswith('_test') or name_str.endswith('_tests'):
        return False
    if name_str.endswith('_suite'):
        return False

    # Skip internal functions
    if name_str.startswith('_'):
        return False
    if name_str.startswith('do_'):
        return False
    if name_str == 'loop' or name_str.endswith('_loop'):
        return False

    # Skip OTP callbacks (they should be exported via behaviours)
    otp_callbacks = {
        'init', 'handle_call', 'handle_cast', 'handle_info',
        'terminate', 'code_change', 'format_status',
        'handle_continue', 'handle_debug',
        'init', 'pre_halt', 'post_halt', 'terminate', 'handle_sync_event',
        'handle_event', 'handle_info', 'code_change', 'format_status',
        'start_link', 'start', 'stop', 'call', 'cast', 'multi_call',
        'abcast', 'bcast', 'register', 'unregister', 'whereis', 'registered',
    }
    if name_str in otp_callbacks:
        return False

    return True


def analyze_file(filepath: Path) -> Dict:
    """Analyze a single Erlang file for export issues."""
    try:
        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read()
    except Exception as e:
        return {'error': str(e)}

    exports = extract_exports(content)
    definitions = extract_definitions(content)

    # Find exported but undefined
    undefined = exports - definitions

    # Find defined but not exported (potential missing exports)
    not_exported = definitions - exports
    public_not_exported = {f for f in not_exported if is_public_function(f[0], f[1])}

    # Find duplicates in exports
    # (Since we use a set, duplicates are automatically removed)
    # Need to check the raw content for duplicate export entries
    duplicate_exports = find_duplicate_exports_in_content(content)

    return {
        'exports': exports,
        'definitions': definitions,
        'undefined': undefined,
        'not_exported': public_not_exported,
        'duplicates': duplicate_exports,
    }


def find_duplicate_exports_in_content(content: str) -> Set[Tuple[str, int]]:
    """Find functions that appear multiple times in export lists."""
    all_exported = []
    pattern = r'-export\s*\(\s*\[\s*([^\]]+)\s*\]\s*\)\s*\.'
    matches = re.findall(pattern, content)
    for match in matches:
        for item in re.split(r',\s*', match.strip()):
            item = item.strip()
            if not item:
                continue
            if '/' in item:
                name, arity = item.split('/', 1)
                try:
                    arity = int(arity)
                    all_exported.append((name, arity))
                except ValueError:
                    pass

    # Count occurrences
    counts = defaultdict(int)
    for item in all_exported:
        counts[item] += 1

    return {item for item, count in counts.items() if count > 1}


def main():
    """Main entry point."""
    if len(sys.argv) < 2:
        print("Usage: analyze_exports.py <directory>...")
        sys.exit(1)

    directories = sys.argv[1:]

    issues_found = False
    total_files = 0

    for directory in directories:
        dir_path = Path(directory)
        if not dir_path.exists():
            print(f"Error: Directory not found: {directory}", file=sys.stderr)
            continue

        # Find all .erl files
        erl_files = [
            f for f in dir_path.rglob('*.erl')
            if '_build' not in str(f) and '_archived' not in str(f)
        ]

        print(f"\nAnalyzing {len(erl_files)} files in {directory}...")

        for filepath in sorted(erl_files):
            total_files += 1
            result = analyze_file(filepath)

            if 'error' in result:
                print(f"{filepath}: ERROR - {result['error']}")
                issues_found = True
                continue

            undefined = result['undefined']
            not_exported = result['not_exported']
            duplicates = result['duplicates']

            if undefined or not_exported or duplicates:
                issues_found = True
                print(f"\n{filepath}:")

                if undefined:
                    sorted_undefined = sorted(undefined, key=lambda x: (x[0], x[1]))
                    print(f"  Exported but undefined: {sorted_undefined}")

                if not_exported:
                    sorted_not_exported = sorted(not_exported, key=lambda x: (x[0], x[1]))
                    print(f"  Defined but not exported (public): {sorted_not_exported}")

                if duplicates:
                    print(f"  Duplicate exports: {sorted(duplicates)}")

    if total_files == 0:
        print("No Erlang files found.")
        sys.exit(1)

    if issues_found:
        print(f"\nIssues found in analyzed files.")
        sys.exit(1)
    else:
        print(f"\nNo issues found in {total_files} files.")
        sys.exit(0)


if __name__ == '__main__':
    main()
