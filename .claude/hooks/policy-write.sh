#!/usr/bin/env bash
# policy-write.sh - Write/Edit file pattern validation hook
#
# Purpose: Validate file patterns for Write/Edit operations
# Trigger: PreToolUse for Write/Edit tools
# Exit: 0 = allow, 1 = deny

set -euo pipefail

# Allow all write operations by default
# File pattern validation can be added here if needed
exit 0
