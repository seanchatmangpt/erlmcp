#!/usr/bin/env python3
"""
json-parse.py - Fallback JSON parser for gate-status-ascii.sh

Simple JSON query tool when jq is not available.
"""

import json
import sys

def main():
    if len(sys.argv) < 2:
        print("Usage: json-parse.py <query> [json_file]", file=sys.stderr)
        sys.exit(1)

    query = sys.argv[1]

    # Read JSON from file or stdin
    if len(sys.argv) >= 3:
        with open(sys.argv[2], 'r') as f:
            data = json.load(f)
    else:
        data = json.load(sys.stdin)

    # Parse query (simplified jq-like syntax)
    parts = query.strip('.').split('.')

    try:
        result = data
        for part in parts:
            if '[' in part:
                # Array access: gates[0]
                key, idx = part.split('[')
                idx = int(idx.rstrip(']'))
                result = result[key][idx]
            elif part:
                result = result[part]

        # Output result
        if isinstance(result, (str, int, float, bool)):
            print(result)
        elif result is None:
            print("")
        else:
            print(json.dumps(result))
    except (KeyError, IndexError, ValueError):
        # Return empty/default on error
        print("")

if __name__ == '__main__':
    main()
