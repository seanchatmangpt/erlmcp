#!/usr/bin/env python3
"""
gate-status-ascii.py - ASCII art visualization of quality gates

Pure Python implementation that works reliably without jq dependency.
"""

import json
import sys
import os
from pathlib import Path

# Colors (ANSI escape codes)
COLORS = {
    'RED': '\033[0;31m',
    'GREEN': '\033[0;32m',
    'YELLOW': '\033[1;33m',
    'BLUE': '\033[0;34m',
    'MAGENTA': '\033[0;35m',
    'CYAN': '\033[0;36m',
    'WHITE': '\033[1;37m',
    'GRAY': '\033[0;90m',
    'NC': '\033[0m'
}

# Box drawing characters
BOX = {
    'TL': '┌', 'TR': '┐', 'BL': '└', 'BR': '┘',
    'H': '─', 'V': '│', 'VR': '├', 'VL': '┤'
}

# Status icons
ICONS = {
    'pass': '✅',
    'fail': '❌',
    'running': '⏳',
    'warning': '⚠️',
    'blocked': '🚫',
    'pending': '⏸️',
    'unknown': '❓'
}

# Status colors
STATUS_COLORS = {
    'pass': 'GREEN',
    'fail': 'RED',
    'running': 'YELLOW',
    'warning': 'YELLOW',
    'blocked': 'MAGENTA',
    'pending': 'GRAY',
    'unknown': 'GRAY'
}

USE_COLOR = True
COMPACT_MODE = False


def color(color_name):
    """Return ANSI color code"""
    if USE_COLOR and color_name in COLORS:
        return COLORS[color_name]
    return ''


def draw_line(width=60):
    """Draw horizontal line"""
    return BOX['H'] * width


def draw_header():
    """Draw dashboard header"""
    width = 60
    print()
    print(f"{BOX['TL']}{draw_line(width)}{BOX['TR']}")

    print(f"{color('CYAN')}{BOX['V']} {'🏭 erlmcp Quality Gates - TCPS Dashboard':<{width-2}} {BOX['V']}{color('NC')}")

    print(f"{BOX['VR']}{draw_line(width)}{BOX['VL']}")


def draw_gate(gate_id, gate_data):
    """Draw single gate row"""
    status = gate_data.get('status', 'unknown')
    name = gate_data.get('name', f'Gate {gate_id}')
    details = gate_data.get('details', '')

    # Truncate details if too long
    if len(details) > 30:
        details = details[:27] + '...'

    icon = ICONS.get(status, ICONS['unknown'])
    status_color = STATUS_COLORS.get(status, 'GRAY')

    print(f"{BOX['V']} {color(status_color)}[{icon}]{color('NC')} {gate_id}. {name:<20} {color('GRAY')}{details:<20}{color('NC')} {BOX['V']}")


def draw_footer(data):
    """Draw footer with overall status"""
    overall = data.get('overall', 'unknown')
    pass_rate = data.get('pass_rate', 0)
    passed = data.get('passed_count', 0)
    failed = data.get('failed_count', 0)
    timestamp = data.get('timestamp', 'Unknown')

    width = 60
    print(f"{BOX['VR']}{draw_line(width)}{BOX['VL']}")

    # Overall status
    overall_icon = ICONS.get(overall, ICONS['unknown'])
    overall_color = STATUS_COLORS.get(overall, 'GRAY')

    overall_text = f"Overall: {overall_icon} {overall.upper()}"
    print(f"{BOX['V']} {color('WHITE')}Overall: {color(overall_color)}{overall_icon} {overall.upper()}{color('NC')}{' '*(width-len(overall_text)-3)} {BOX['V']}")

    # Metrics
    rate_color = 'GREEN' if pass_rate >= 90 else ('YELLOW' if pass_rate >= 70 else 'RED')

    metrics = f"Pass Rate: {color(rate_color)}{pass_rate}%{color('NC')}  │  Passed: {color('GREEN')}{passed}{color('NC')}  │  Failed: {color('RED')}{failed}{color('NC')}"
    print(f"{BOX['V']} {color('GRAY')}Pass Rate: {color(rate_color)}{pass_rate}%{color('GRAY')}  │  Passed: {color('GREEN')}{passed}{color('GRAY')}  │  Failed: {color('RED')}{failed}{color('NC')}{' '*19} {BOX['V']}")

    # Timestamp
    print(f"{BOX['V']} {color('GRAY')}Last updated: {timestamp}{color('NC')}{' '*(width-len(timestamp)-17)} {BOX['V']}")

    print(f"{BOX['BL']}{draw_line(width)}{BOX['BR']}")
    print()


def draw_compact(data):
    """Draw compact version"""
    print()
    print(f"{color('CYAN')}🏭 erlmcp Quality Gates{color('NC')}")
    print("━" * 30)

    gates = data.get('gates', [])
    for gate in gates:
        gate_id = gate.get('id')
        name = gate.get('name', f'Gate {gate_id}')
        status = gate.get('status', 'unknown')
        details = gate.get('details', '')

        icon = ICONS.get(status, ICONS['unknown'])
        status_color = STATUS_COLORS.get(status, 'GRAY')

        print(f"{color(status_color)}{icon} {gate_id}. {name:<25}{color('GRAY')}{details}{color('NC')}")

    overall = data.get('overall', 'unknown')
    overall_icon = ICONS.get(overall, ICONS['unknown'])
    overall_color = STATUS_COLORS.get(overall, 'GRAY')

    print("━" * 30)
    print(f"{color(overall_color)}Overall: {overall_icon} {overall.upper()}{color('NC')}")
    print()


def display_status(data):
    """Main display function"""
    if COMPACT_MODE:
        draw_compact(data)
    else:
        draw_header()

        gates = data.get('gates', [])
        for gate in gates:
            gate_id = gate.get('id')
            draw_gate(gate_id, gate)

        draw_footer(data)


def main():
    global USE_COLOR, COMPACT_MODE

    json_file = None

    # Parse arguments
    for arg in sys.argv[1:]:
        if arg == '--compact':
            COMPACT_MODE = True
        elif arg == '--no-color':
            USE_COLOR = False
        elif arg == '--color':
            USE_COLOR = True
        elif arg.startswith('--json-file='):
            json_file = arg.split('=', 1)[1]
        elif arg == '--help':
            print(__doc__)
            print("\nUsage: gate-status-ascii.py [--compact] [--no-color] [--json-file=FILE]")
            return 0

    # Detect TTY
    if not sys.stdout.isatty():
        USE_COLOR = False

    # Read JSON
    try:
        if json_file and json_file != '/dev/stdin':
            with open(json_file, 'r') as f:
                data = json.load(f)
        else:
            data = json.load(sys.stdin)
    except FileNotFoundError:
        print(f"Error: JSON file not found: {json_file}", file=sys.stderr)
        return 1
    except json.JSONDecodeError as e:
        print(f"Error: Invalid JSON: {e}", file=sys.stderr)
        return 1

    display_status(data)
    return 0


if __name__ == '__main__':
    sys.exit(main())
