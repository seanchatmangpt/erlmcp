#!/usr/bin/env bash
# policy-websearch.sh - WebSearch domain filtering hook
#
# Purpose: Validate and filter web search domains
# Trigger: PreToolUse for WebSearch tool
# Exit: 0 = allow, 1 = deny

set -euo pipefail

# Allow all web searches by default
# Domain filtering can be added here if needed
exit 0
