# Claude Code Hooks - Configuration Examples

**Version:** 2.0.0
**Project:** erlmcp
**Last Updated:** 2026-01-28

This document provides ready-to-use configuration examples for Claude Code hooks in erlmcp.

---

## Table of Contents

1. [Basic Configuration](#basic-configuration)
2. [Full Configuration (Production)](#full-configuration-production)
3. [Development Configuration](#development-configuration)
4. [CI/CD Configuration](#cicd-configuration)
5. [Minimal Configuration](#minimal-configuration)
6. [Hook-Specific Examples](#hook-specific-examples)

---

## Basic Configuration

**File:** `.claude/settings.json`

Minimal setup with essential hooks enabled.

```json
{
  "env": {
    "CLAUDE_FLOW_HOOKS_ENABLED": "true"
  },
  "permissions": {
    "allow": [
      "Bash(rebar3 *)",
      "Bash(make *)",
      "Bash(git status)",
      "Bash(git diff *)",
      "Bash(git add *)",
      "Bash(git commit *)"
    ],
    "deny": [
      "Bash(rm -rf /)",
      "Bash(eval *)"
    ]
  },
  "hooks": {
    "PreToolUse": [],
    "PostToolUse": [],
    "Stop": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "echo 'Session ended'"
          }
        ]
      }
    ]
  }
}
```

---

## Full Configuration (Production)

**File:** `.claude/settings.json`

Complete production configuration with all hooks and Claude Flow integration.

```json
{
  "env": {
    "CLAUDE_FLOW_AUTO_COMMIT": "false",
    "CLAUDE_FLOW_AUTO_PUSH": "false",
    "CLAUDE_FLOW_HOOKS_ENABLED": "true",
    "CLAUDE_FLOW_TELEMETRY_ENABLED": "true",
    "CLAUDE_FLOW_REMOTE_EXECUTION": "true",
    "CLAUDE_FLOW_CHECKPOINTS_ENABLED": "true",
    "CLAUDE_FLOW_DEBUG": "false"
  },
  "permissions": {
    "allow": [
      "Bash(npx claude-flow *)",
      "Bash(npm run lint)",
      "Bash(npm run test:*)",
      "Bash(npm test *)",
      "Bash(rebar3 *)",
      "Bash(make *)",
      "Bash(git status)",
      "Bash(git diff *)",
      "Bash(git log *)",
      "Bash(git add *)",
      "Bash(git commit *)",
      "Bash(git push)",
      "Bash(git config *)",
      "Bash(git tag *)",
      "Bash(git branch *)",
      "Bash(git checkout *)",
      "Bash(git stash *)",
      "Bash(jq *)",
      "Bash(node *)",
      "Bash(which *)",
      "Bash(pwd)",
      "Bash(ls *)",
      "Bash(cat *)",
      "Bash(grep *)",
      "Bash(find *)",
      "Bash(./tools/*.sh)",
      "Bash(./scripts/*.sh)"
    ],
    "deny": [
      "Bash(rm -rf /)",
      "Bash(curl * | bash)",
      "Bash(wget * | sh)",
      "Bash(eval *)",
      "Bash(dd *)",
      "Bash(mkfs *)",
      "Bash(format *)"
    ]
  },
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "cat | jq -r '.tool_input.command // empty' | tr '\\n' '\\0' | xargs -0 -I {} npx claude-flow@alpha hooks pre-command --command '{}' --validate-safety true --prepare-resources true"
          }
        ]
      },
      {
        "matcher": "Write|Edit|MultiEdit",
        "hooks": [
          {
            "type": "command",
            "command": "cat | jq -r '.tool_input.file_path // .tool_input.path // empty' | tr '\\n' '\\0' | xargs -0 -I {} npx claude-flow@alpha hooks pre-edit --file '{}' --auto-assign-agents true --load-context true"
          }
        ]
      }
    ],
    "PostToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "cat | jq -r '.tool_input.command // empty' | tr '\\n' '\\0' | xargs -0 -I {} npx claude-flow@alpha hooks post-command --command '{}' --track-metrics true --store-results true"
          }
        ]
      },
      {
        "matcher": "Write|Edit|MultiEdit",
        "hooks": [
          {
            "type": "command",
            "command": "cat | jq -r '.tool_input.file_path // .tool_input.path // empty' | tr '\\n' '\\0' | xargs -0 -I {} npx claude-flow@alpha hooks post-edit --file '{}' --format true --update-memory true"
          }
        ]
      }
    ],
    "PreCompact": [
      {
        "matcher": "manual",
        "hooks": [
          {
            "type": "command",
            "command": "/bin/bash -c 'INPUT=$(cat); CUSTOM=$(echo \"$INPUT\" | jq -r \".custom_instructions // \\\"\\\"\"); echo \"🔄 PreCompact Guidance:\"; echo \"📋 IMPORTANT: Review CLAUDE.md in project root for:\"; echo \"   • 10 Erlang/OTP agents (consolidated from 57)\"; echo \"   • 30 TCPS commands: 26 manufacturing + 4 top-level\"; echo \"   • Toyota Production System (TCPS) with Japanese terminology\"; echo \"   • Manufacturing flow: Pull → Heijunka → Kanban → Build → Jidoka → Andon → Kaizen → Receipt\"; echo \"   • Critical concurrent execution rules (GOLDEN RULE: 1 MESSAGE = ALL OPERATIONS)\"; if [ -n \"$CUSTOM\" ]; then echo \"🎯 Custom compact instructions: $CUSTOM\"; fi; echo \"✅ Ready for compact operation\"'"
          }
        ]
      },
      {
        "matcher": "auto",
        "hooks": [
          {
            "type": "command",
            "command": "/bin/bash -c 'echo \"🔄 Auto-Compact Guidance (Context Window Full):\"; echo \"📋 CRITICAL: Before compacting, ensure you understand:\"; echo \"   • 10 Erlang/OTP agents in .claude/agents/ (consolidated)\"; echo \"   • 30 TCPS commands: /tcps-*, /poka-yoke-*, /work-order-*, /sku-*, /standard-work-*\"; echo \"   • Toyota manufacturing flow with Japanese terms (自働化, 看板, 平準化, etc.)\"; echo \"   • Concurrent execution patterns from CLAUDE.md\"; echo \"⚡ Apply GOLDEN RULE: Always batch operations in single messages\"; echo \"✅ Auto-compact proceeding with TCPS manufacturing context\"'"
          }
        ]
      }
    ],
    "Stop": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "npx claude-flow@alpha hooks session-end --generate-summary true --persist-state true --export-metrics true"
          }
        ]
      }
    ]
  },
  "includeCoAuthoredBy": true,
  "enabledMcpjsonServers": ["claude-flow", "ruv-swarm"]
}
```

---

## Development Configuration

**File:** `.claude/settings.json`

Development-friendly configuration with relaxed validation and debugging enabled.

```json
{
  "env": {
    "CLAUDE_FLOW_HOOKS_ENABLED": "true",
    "CLAUDE_FLOW_TELEMETRY_ENABLED": "true",
    "CLAUDE_FLOW_DEBUG": "true",
    "CLAUDE_FLOW_STRICT_MODE": "false"
  },
  "permissions": {
    "allow": [
      "Bash(rebar3 *)",
      "Bash(make *)",
      "Bash(git *)",
      "Bash(./tools/*.sh)"
    ],
    "deny": [
      "Bash(rm -rf /)"
    ]
  },
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Write|Edit|MultiEdit",
        "hooks": [
          {
            "type": "command",
            "command": "echo 'Editing file...'"
          }
        ]
      }
    ],
    "PostToolUse": [
      {
        "matcher": "Write|Edit|MultiEdit",
        "hooks": [
          {
            "type": "command",
            "command": "cat | jq -r '.tool_input.file_path // empty' | xargs -I {} bash -c 'echo \"Formatted: {}\"'"
          }
        ]
      }
    ],
    "Stop": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "echo 'Dev session ended - logs saved'"
          }
        ]
      }
    ]
  }
}
```

---

## CI/CD Configuration

**File:** `.claude/settings.json`

Strict configuration for CI/CD pipelines with all validations enabled.

```json
{
  "env": {
    "CLAUDE_FLOW_HOOKS_ENABLED": "true",
    "CLAUDE_FLOW_TELEMETRY_ENABLED": "true",
    "CLAUDE_FLOW_STRICT_MODE": "true",
    "CLAUDE_FLOW_FAIL_FAST": "true",
    "CI": "true"
  },
  "permissions": {
    "allow": [
      "Bash(rebar3 compile)",
      "Bash(rebar3 eunit)",
      "Bash(rebar3 ct)",
      "Bash(rebar3 dialyzer)",
      "Bash(rebar3 xref)",
      "Bash(rebar3 cover)",
      "Bash(make validate*)",
      "Bash(make test*)",
      "Bash(./tools/quality-gate.sh)",
      "Bash(./tools/claude-md-enforcer.sh)"
    ],
    "deny": [
      "Bash(git push)",
      "Bash(git commit)",
      "Bash(rm *)"
    ]
  },
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "cat | jq -r '.tool_input.command // empty' | xargs -I {} bash -c 'echo \"[CI] Running: {}\"'"
          }
        ]
      }
    ],
    "PostToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "cat | jq -r '.tool_input.command // empty' | xargs -I {} bash -c 'echo \"[CI] Completed: {}\"'"
          }
        ]
      }
    ],
    "Stop": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "bash -c 'echo \"[CI] Pipeline ended - exit code: $?\"'"
          }
        ]
      }
    ]
  }
}
```

---

## Minimal Configuration

**File:** `.claude/settings.json`

Absolute minimal configuration with no hooks (for testing/debugging).

```json
{
  "env": {
    "CLAUDE_FLOW_HOOKS_ENABLED": "false"
  },
  "permissions": {
    "allow": ["Bash(*)"],
    "deny": []
  },
  "hooks": {}
}
```

---

## Hook-Specific Examples

### Example 1: Pre-Edit Hook with File Type Detection

```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Write|Edit|MultiEdit",
        "hooks": [
          {
            "type": "command",
            "command": "cat | jq -r '.tool_input.file_path // empty' | xargs -I {} bash -c 'FILE=\"{}\"; if [[ \"$FILE\" == *.erl ]]; then echo \"Editing Erlang module: $FILE\"; elif [[ \"$FILE\" == *.hrl ]]; then echo \"Editing Erlang header: $FILE\"; else echo \"Editing file: $FILE\"; fi'"
          }
        ]
      }
    ]
  }
}
```

---

### Example 2: Post-Edit Hook with Auto-Formatting

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Write|Edit|MultiEdit",
        "hooks": [
          {
            "type": "command",
            "command": "cat | jq -r '.tool_input.file_path // empty' | xargs -I {} bash -c 'FILE=\"{}\"; if [[ \"$FILE\" == *.erl ]]; then rebar3 format --files \"$FILE\" 2>/dev/null || echo \"Format skipped: $FILE\"; fi'"
          }
        ]
      }
    ]
  }
}
```

---

### Example 3: Pre-Commit Hook with Quality Gate

```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "cat | jq -r '.tool_input.command // empty' | grep -q 'git commit' && bash -c './tools/claude-md-enforcer.sh || exit 1' || true"
          }
        ]
      }
    ]
  }
}
```

---

### Example 4: Post-Test Hook with Coverage Report

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "cat | jq -r '.tool_input.command // empty' | grep -q 'rebar3 eunit' && bash -c 'echo \"Tests completed. Generating coverage...\"; rebar3 cover 2>/dev/null | grep -o \"[0-9]\\+%\" | head -1 | xargs -I {} echo \"Coverage: {}\"' || true"
          }
        ]
      }
    ]
  }
}
```

---

### Example 5: Stop Hook with Summary

```json
{
  "hooks": {
    "Stop": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "bash -c 'echo \"═══════════════════════════════════════\"; echo \"Session Summary\"; echo \"═══════════════════════════════════════\"; echo \"Project: erlmcp\"; echo \"Time: $(date)\"; echo \"Working directory: $(pwd)\"; echo \"Git branch: $(git branch --show-current 2>/dev/null || echo \"N/A\")\"; echo \"Git status: $(git status --short 2>/dev/null | wc -l) files modified\"; echo \"═══════════════════════════════════════\"'"
          }
        ]
      }
    ]
  }
}
```

---

### Example 6: Conditional Hook Execution

```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "cat | jq -r '.tool_input.command // empty' | xargs -I {} bash -c 'CMD=\"{}\"; if [[ \"$CMD\" == *\"make\"* ]]; then echo \"[HOOK] Running Makefile target: $CMD\"; fi; if [[ \"$CMD\" == *\"rebar3\"* ]]; then echo \"[HOOK] Running rebar3 command: $CMD\"; fi'"
          }
        ]
      }
    ]
  }
}
```

---

### Example 7: Environment-Specific Hook

```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "bash -c 'if [[ -n \"$CI\" ]]; then echo \"[CI MODE] Strict validation enabled\"; else echo \"[DEV MODE] Standard validation\"; fi'"
          }
        ]
      }
    ]
  }
}
```

---

### Example 8: Multi-Step Post-Edit Hook

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Write|Edit|MultiEdit",
        "hooks": [
          {
            "type": "command",
            "command": "cat | jq -r '.tool_input.file_path // empty' | xargs -I {} bash -c 'FILE=\"{}\"; echo \"[HOOK] Processing $FILE...\"; if [[ \"$FILE\" == *.erl ]]; then echo \"1/3 Formatting...\"; rebar3 format --files \"$FILE\" 2>/dev/null; echo \"2/3 Running dialyzer...\"; rebar3 dialyzer --files \"$FILE\" 2>/dev/null; echo \"3/3 Done\"; fi'"
          }
        ]
      }
    ]
  }
}
```

---

### Example 9: Telemetry Hook

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "cat | jq -r '.tool_input.command // empty' | xargs -I {} bash -c 'echo \"{}\" >> /tmp/claude_commands.log; echo \"[TELEMETRY] Logged command to /tmp/claude_commands.log\"'"
          }
        ]
      }
    ]
  }
}
```

---

### Example 10: Safety Validation Hook

```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "cat | jq -r '.tool_input.command // empty' | xargs -I {} bash -c 'CMD=\"{}\"; DANGEROUS=(\"rm -rf\" \"dd\" \"mkfs\" \"format\" \"eval\"); for PATTERN in \"${DANGEROUS[@]}\"; do if [[ \"$CMD\" == *\"$PATTERN\"* ]]; then echo \"[SAFETY] BLOCKED dangerous command: $CMD\"; exit 1; fi; done; echo \"[SAFETY] Command validated: $CMD\"'"
          }
        ]
      }
    ]
  }
}
```

---

## Installation Script

Create this as `.claude/install-settings.sh` to quickly switch configurations:

```bash
#!/usr/bin/env bash
# Install Claude Code settings for erlmcp

set -e

CONFIGS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/configs"
SETTINGS_FILE=".claude/settings.json"

echo "Available configurations:"
echo "  1. basic      - Minimal hooks"
echo "  2. full       - Production (recommended)"
echo "  3. dev        - Development (debug enabled)"
echo "  4. ci         - CI/CD strict mode"
echo "  5. minimal    - No hooks (testing)"

read -p "Select configuration [1-5]: " CHOICE

case $CHOICE in
  1) CONFIG="basic" ;;
  2) CONFIG="full" ;;
  3) CONFIG="dev" ;;
  4) CONFIG="ci" ;;
  5) CONFIG="minimal" ;;
  *) echo "Invalid choice"; exit 1 ;;
esac

if [[ -f "$SETTINGS_FILE" ]]; then
  BACKUP="$SETTINGS_FILE.backup.$(date +%Y%m%d_%H%M%S)"
  cp "$SETTINGS_FILE" "$BACKUP"
  echo "Backed up existing settings to: $BACKUP"
fi

cat > "$SETTINGS_FILE" < "$CONFIGS_DIR/$CONFIG.json"
echo "✅ Installed $CONFIG configuration"
echo "Location: $SETTINGS_FILE"
```

---

## Testing Hooks

### Manual Hook Test

```bash
# Test pre-commit hook
./.claude/hooks/pre-commit-validate.sh

# Test post-task hook
./.claude/hooks/post-task-validate.sh task-123 "Test task"

# Test CLAUDE.md enforcer
./tools/claude-md-enforcer.sh
```

### Integration Test

```bash
# Create test commit (should trigger hooks)
echo "// test" >> test/dummy.erl
git add test/dummy.erl
git commit -m "Test hooks"

# Hooks should run automatically
# If validation fails, commit is blocked
```

---

## Environment Variables Reference

| Variable | Default | Purpose |
|----------|---------|---------|
| `CLAUDE_FLOW_HOOKS_ENABLED` | `true` | Enable/disable all hooks |
| `CLAUDE_FLOW_TELEMETRY_ENABLED` | `true` | Track execution metrics |
| `CLAUDE_FLOW_DEBUG` | `false` | Enable debug logging |
| `CLAUDE_FLOW_STRICT_MODE` | `false` | Fail-fast on violations |
| `CLAUDE_FLOW_AUTO_COMMIT` | `false` | Auto-commit changes |
| `CLAUDE_FLOW_AUTO_PUSH` | `false` | Auto-push commits |
| `CLAUDE_FLOW_CHECKPOINTS_ENABLED` | `true` | Enable state snapshots |
| `CI` | - | CI/CD mode flag |

---

## Permissions Reference

### Allowed Patterns

```json
"allow": [
  "Bash(rebar3 *)",           // All rebar3 commands
  "Bash(make *)",             // All make targets
  "Bash(git status)",         // Read-only git
  "Bash(./tools/*.sh)",       // Project scripts
  "Bash(ls *)",               // File listing
  "Bash(cat *)",              // File reading
  "Bash(grep *)",             // Search
  "Bash(jq *)"                // JSON processing
]
```

### Denied Patterns

```json
"deny": [
  "Bash(rm -rf /)",           // Prevent catastrophic deletion
  "Bash(curl * | bash)",      // Prevent remote code execution
  "Bash(eval *)",             // Prevent eval injection
  "Bash(dd *)",               // Prevent disk writes
  "Bash(mkfs *)",             // Prevent formatting
  "Bash(format *)"            // Prevent formatting
]
```

---

## Best Practices

1. **Start with Basic:** Use basic configuration first, add complexity as needed
2. **Test Hooks:** Always test hooks manually before committing
3. **Backup Settings:** Keep backups of working configurations
4. **Version Control:** Commit `.claude/settings.json` to repo
5. **Document Custom Hooks:** Add comments explaining custom hook logic
6. **Security First:** Always deny dangerous commands in permissions
7. **Environment-Aware:** Use env vars for dev/CI/prod differences
8. **Log Everything:** Enable telemetry for debugging and auditing

---

## Troubleshooting

### Hooks Not Running

```bash
# Check if hooks enabled
jq '.env.CLAUDE_FLOW_HOOKS_ENABLED' .claude/settings.json

# Should output: "true"
```

### Permission Denied

```bash
# Check permissions list
jq '.permissions.allow[]' .claude/settings.json

# Add missing command to allow list
```

### Hook Failing

```bash
# Enable debug mode
jq '.env.CLAUDE_FLOW_DEBUG = "true"' .claude/settings.json > /tmp/settings.json
mv /tmp/settings.json .claude/settings.json

# Check logs
cat /tmp/claude_hooks.log
```

---

## Additional Resources

- **Main Guide:** `docs/CLAUDE_CODE_HOOKS_GUIDE.md`
- **CLAUDE.md:** Quality standards (project root)
- **Hook Scripts:** `.claude/hooks/`
- **Makefile Targets:** `make help`

---

**Last Updated:** 2026-01-28
**Version:** 2.0.0
**Maintainer:** erlmcp core team
