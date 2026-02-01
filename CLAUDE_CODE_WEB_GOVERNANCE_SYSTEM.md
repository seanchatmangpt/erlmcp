# Claude Code Web Governance System for erlmcp

**Thesis**: Don't reinvent orchestration. Use Claude Code Web's native primitives (hooks, skills, subagents, settings scopes) as the **programmable runtime governor**.

**Pattern**: Policy → Execution → Verification → Receipt

---

## Part 1: Native Primitives (The Real Control Plane)

### 1. Sandbox Boundaries
- **Network**: Proxy-enforced allowlist (GitHub, hex.pm, official package managers)
- **Filesystem**: Session ephemeral `/home/user/erlmcp`, can mount persistent areas
- **Git**: Custom proxy validates push target (repo + branch only)
- **Env**: `CLAUDE_CODE_REMOTE=true` signals cloud execution

### 2. Settings Scopes (.claude/settings.json)
- Project-wide policy (committed)
- Can be overridden in `.claude/settings.local.json` (gitignored)
- Deterministic precedence; parsed and enforced by Claude Code runtime

### 3. Hooks (The Programmable Lifecycle)
**Event flow**:
```
SessionStart → [UserPromptSubmit*] → PreToolUse → [PostToolUse]* → PreCompact → Stop → SessionEnd
```

**Hook mechanics**:
- `type: "command"` — synchronous bash script, returns exit code + stdout
- `type: "agent"` — spawn subagent with tool access, return JSON decision
- `async: true` — run in background (PostToolUse only)
- `matcher` — regex on event context (tool name, path, etc.)
- Can block, ask, allow, modify inputs

### 4. Skills (Reusable Procedures + Slash Commands)
- `.claude/skills/<name>/SKILL.md` + supporting scripts
- Appears as `/name` slash command
- Bundled frontmatter controls + preload capability
- **Perfect for**: OTP management, cache warming, bootstrap sequences

### 5. Subagents (Locked-Down Specialists)
- Explicit tool allowlists/denylists
- Permission modes (read/write/exec)
- Preload skills
- Scoped hooks
- **Perfect for**: role-based execution (verify-only vs build vs deploy)

### 6. Plugins (Org-Wide Distribution)
- Package (skills + hooks + subagents + MCP servers)
- Distribute via marketplace
- Single `/plugin install` rollout to entire org

### 7. MCP Servers (Optional Integration)
- Custom tools appear as `mcp__<server>__<tool>`
- Governable via hooks (same PreToolUse mechanism)
- **Perfect for**: GitHub metadata, artifact storage, secret vaults

---

## Part 2: The "Policy → Execution → Verification → Receipt" Pattern

### Layer 1: Policy (PreToolUse Hook)

**What it does**:
- Intercepts *all* tool calls before execution
- Can block, ask user, allow, or **rewrite input**
- Returns structured decision: `{ "permissionDecision": "allow"|"ask"|"deny", "updatedInput": {...} }`

**Use cases**:
```bash
# .claude/hooks/policy-bash.sh
PreToolUse event arrives with Bash tool + command

1. Check: Is command git/GitHub-related?
   → YES: allow (network proxy + Git custom proxy will enforce)
   → NO: Is command deterministic + filesystem-safe?
      → YES: allow
      → NO: deny with explanation + suggest safer alternative
```

**Concrete example for OTP**:
```bash
# Intercept Bash calls
if [[ "$TOOL" == "Bash" ]]; then
  if [[ "$COMMAND" =~ ^(wget|curl) ]]; then
    # Only allow downloads from allowlisted domains
    if [[ "$COMMAND" =~ (github.com|hex.pm|erlang-solutions.com) ]]; then
      echo '{"permissionDecision": "allow"}'
    else
      echo '{"permissionDecision": "deny", "reason": "Domain not allowlisted"}'
    fi
  elif [[ "$COMMAND" =~ ^(erl|rebar3|make) ]]; then
    # Always allow build commands
    echo '{"permissionDecision": "allow"}'
  fi
fi
```

### Layer 2: Execution (SessionStart Skill + Hooks)

**SessionStart hook** (idempotent, once per session):
```bash
# .claude/hooks/SessionStart.sh

set -euo pipefail

# 1. Detect OTP version
if ! command -v erl &> /dev/null || \
   ! erl -noshell -eval 'erlang:system_info(otp_release)' | grep -q "28"; then
  # 2. Download OTP 28 from GitHub (GitHub is allowlisted)
  wget https://github.com/erlang/otp/releases/download/OTP-28.3.1/otp_src_28.3.1.tar.gz
  tar xzf otp_src_28.3.1.tar.gz
  cd otp_src_28.3.1 && ./configure && make install
fi

# 3. Setup environment (persist via CLAUDE_ENV_FILE)
cat >> $CLAUDE_ENV_FILE <<'ENVEOF'
export PATH=/opt/erlang-28/bin:$PATH
export ERLMCP_PROFILE=cloud
export ERLMCP_BUILD_HASH=$(git rev-parse HEAD)
ENVEOF

# 4. Pre-compile core
TERM=dumb rebar3 compile apps/erlmcp_core

# 5. Declare constraint (file output)
echo "SessionStart complete. OTP 28 ready. Build hash: $(git rev-parse HEAD)"
```

**Env vars persist** via `CLAUDE_ENV_FILE` — later Bash commands inherit them.

### Layer 3: Verification (Stop Agent Hook)

**What it does**:
- Fired when agent tries to stop/complete
- Spawns a **verifying subagent** with tool access
- Returns `{ "ok": true/false }`
- If false, blocking error

**Stop hook configuration**:
```json
{
  "hooks": {
    "Stop": [
      {
        "hooks": [
          {
            "type": "agent",
            "prompt": "Verify that OTP 28.3.1 is installed, erlmcp compiles, and unit tests pass. Return {\"ok\": true} if all pass, {\"ok\": false, \"reason\": \"...\"} if any fail.",
            "timeout": 120
          }
        ]
      }
    ]
  }
}
```

**Verifying subagent** (locked-down):
```yaml
# .claude/agents/verifier.md
---
name: "OTP Build Verifier"
toolAccess:
  - Bash (constrained to read-only + test commands)
  - Read (logs, config files only)
permissionMode: "execute"  # Can run tests, but not modify code
skills:
  - preload: ["otp-manager"]  # Reuse OTP skill
---

Your role: Verify the OTP build is complete and working.

1. Check `erl -noshell -eval '...' | grep "28"` — OTP version
2. Run `rebar3 compile` — does it complete?
3. Run `rebar3 eunit --application=erlmcp_core` — do tests pass?

Return JSON:
```json
{
  "ok": true|false,
  "reason": "OTP 28.3.1 installed, all tests pass" | "OTP version mismatch"
}
```
```

### Layer 4: Receipt (SessionEnd Hook + PostToolUse Logging)

**SessionEnd hook** (audit trail):
```bash
# .claude/hooks/receipt.sh

set -euo pipefail

RECEIPT_FILE=".erlmcp/receipts/$(date +%s).json"
mkdir -p .erlmcp/receipts

cat > "$RECEIPT_FILE" <<EOF
{
  "session_id": "${SESSION_ID}",
  "timestamp": "$(date -Iseconds)",
  "otp_version": "$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)])' 2>/dev/null || echo 'unknown')",
  "erlmcp_version": "$(cat VERSION 2>/dev/null || git describe --tags 2>/dev/null || echo 'unknown')",
  "build_hash": "$(git rev-parse HEAD)",
  "quality_gates": {
    "compile": "pass|fail",
    "eunit": "pass|fail",
    "ct": "pass|fail"
  },
  "commands_executed": [],
  "errors": [],
  "cost_estimate": "\$0.XX"
}
EOF

echo "Receipt written to $RECEIPT_FILE"

# Archive transcript if available
if [ -f "$TRANSCRIPT_PATH" ]; then
  cp "$TRANSCRIPT_PATH" ".erlmcp/transcripts/session_${SESSION_ID}.log"
fi
```

**PostToolUse logging** (continuous):
```bash
# .claude/hooks/post-write-ci.sh
# Async hook that runs when Edit/Write completes

if [[ "$TOOL" == "Write" || "$TOOL" == "Edit" ]]; then
  FILE="$SUBJECT"
  if [[ "$FILE" =~ \.(erl|hrl|app\.src)$ ]]; then
    # Trigger incremental compile + test
    (
      cd /home/user/erlmcp
      TERM=dumb rebar3 compile 2>&1 >> .erlmcp/build.log
      if [ $? -eq 0 ]; then
        rebar3 eunit --module=$(basename "$FILE" .erl)_tests 2>&1 >> .erlmcp/test.log || true
      fi
    ) &
    # Returns immediately (async: true)
  fi
fi
```

---

## Part 3: Concrete Implementation for erlmcp OTP Workflow

### File Structure
```
.claude/
├── settings.json                    # Project-wide policy
├── settings.local.json              # (gitignored) personal overrides
├── hooks/
│   ├── SessionStart.sh              # OTP bootstrap
│   ├── policy-bash.sh               # Network + FS governance
│   ├── policy-websearch.sh          # Search domain filtering
│   ├── post-write-ci.sh             # Async CI triggering
│   ├── receipt.sh                   # Audit trail
│   └── pre-compact.sh               # Re-inject invariants
├── skills/
│   └── otp-manager/
│       ├── SKILL.md                 # Slash command definition
│       ├── otp_fetch_build.sh       # Download + compile OTP 28
│       ├── otp_verify.sh            # Check version
│       └── otp_clean.sh             # Cleanup
└── agents/
    ├── verifier.md                  # Locked-down verification subagent
    ├── build-engineer.md            # Can Bash + Write in src/
    └── release-scout.md             # Read-only, WebSearch only
```

### settings.json (Project Policy)

```json
{
  "permissions": {
    "deny": [
      "Read(./.env)",
      "Read(./.env.local)",
      "Read(./secrets/**)",
      "Write(./scripts/deploy.sh)"
    ]
  },

  "hooks": {
    "SessionStart": [
      {
        "matcher": "startup|resume",
        "hooks": [
          {
            "type": "command",
            "command": "./.claude/hooks/SessionStart.sh",
            "timeout": 600
          }
        ]
      }
    ],

    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "./.claude/hooks/policy-bash.sh",
            "timeout": 10
          }
        ]
      },
      {
        "matcher": "WebSearch",
        "hooks": [
          {
            "type": "command",
            "command": "./.claude/hooks/policy-websearch.sh",
            "timeout": 5
          }
        ]
      }
    ],

    "PostToolUse": [
      {
        "matcher": "Write|Edit",
        "hooks": [
          {
            "type": "command",
            "command": "./.claude/hooks/post-write-ci.sh",
            "async": true,
            "timeout": 120
          }
        ]
      }
    ],

    "Stop": [
      {
        "hooks": [
          {
            "type": "agent",
            "prompt": "Verify: (1) OTP 28.3.1+ installed, (2) erlmcp compiles, (3) unit tests pass. Return {\"ok\": true} if all pass, else {\"ok\": false, \"reason\": \"...\"}.",
            "subagent": "verifier",
            "timeout": 120
          }
        ]
      }
    ],

    "SessionEnd": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "./.claude/hooks/receipt.sh",
            "timeout": 30
          }
        ]
      }
    ]
  },

  "subagents": {
    "verifier": {
      "toolAccess": {
        "allow": ["Bash", "Read"],
        "deny": ["Write", "Edit", "Delete"]
      },
      "permissionMode": "execute",
      "skills": ["otp-manager"]
    },
    "build-engineer": {
      "toolAccess": {
        "allow": ["Bash", "Read", "Write"],
        "constraint": "Write only in apps/erlmcp_*/src"
      },
      "permissionMode": "write",
      "skills": ["otp-manager"]
    },
    "release-scout": {
      "toolAccess": {
        "allow": ["Read", "WebSearch"],
        "deny": ["Bash", "Write", "Edit", "Delete"]
      },
      "permissionMode": "read"
    }
  }
}
```

### .claude/hooks/policy-bash.sh (Network Governance)

```bash
#!/usr/bin/env bash
set -euo pipefail

# Input: tool call event (JSON stdin)
INPUT=$(cat)

COMMAND=$(echo "$INPUT" | jq -r '.command // ""')
TOOL=$(echo "$INPUT" | jq -r '.tool // ""')

if [[ "$TOOL" != "Bash" ]]; then
  echo '{"permissionDecision": "allow"}'
  exit 0
fi

# Allowlisted domains/patterns
ALLOWLIST=(
  "github.com"
  "hex.pm"
  "erlang-solutions.com"
  "packages.erlang-solutions.com"
  "github.com/erlang/otp"
)

# Dangerous commands
DANGEROUS=(
  "rm -rf /"
  "sudo"
  "> /dev/sda"
)

# Check if command matches dangerous pattern
for pattern in "${DANGEROUS[@]}"; do
  if [[ "$COMMAND" =~ $pattern ]]; then
    echo "{\"permissionDecision\": \"deny\", \"reason\": \"Dangerous command pattern\"}"
    exit 0
  fi
done

# Check if command is network-related
if [[ "$COMMAND" =~ (wget|curl|git.*clone|git.*push) ]]; then
  # Check if URL is allowlisted
  ALLOWED=false
  for domain in "${ALLOWLIST[@]}"; do
    if [[ "$COMMAND" =~ $domain ]]; then
      ALLOWED=true
      break
    fi
  done

  if $ALLOWED; then
    echo '{"permissionDecision": "allow"}'
  else
    echo "{\"permissionDecision\": \"ask\", \"reason\": \"Network URL not in allowlist. Allow? ${COMMAND:0:100}...\"}"
  fi
  exit 0
fi

# Build commands (erl, rebar3, make) always allowed
if [[ "$COMMAND" =~ ^(erl|rebar3|make|TERM=) ]]; then
  echo '{"permissionDecision": "allow"}'
  exit 0
fi

# Default: ask
echo "{\"permissionDecision\": \"ask\", \"reason\": \"Unrecognized command\"}"
```

### .claude/skills/otp-manager/SKILL.md

```markdown
# OTP Manager

Manage Erlang/OTP installation and verification.

## Usage

- `/otp-manager fetch-build` — Download + build OTP 28.3.1
- `/otp-manager verify` — Check OTP version
- `/otp-manager clean` — Remove build artifacts

## Example

\`\`\`
/otp-manager fetch-build
\`\`\`

This will:
1. Download OTP 28.3.1 from GitHub
2. Configure with standard flags
3. Build and install to /opt/erlang-28
4. Verify with `erl -noshell -eval ...`
```

### .claude/agents/verifier.md

```yaml
---
name: "OTP Build Verifier"
description: "Locked-down verification subagent. Read-only + Bash (execute tests only)."

toolAccess:
  allow:
    - "Bash"
    - "Read"
  deny:
    - "Write"
    - "Edit"
    - "Delete"
    - "WebSearch"

bashConstraints:
  "allow":
    - "^erl -noshell"
    - "^rebar3 eunit"
    - "^rebar3 compile"
  "deny":
    - "sudo"
    - "apt-get|yum|brew"

permissionMode: "execute"

skills:
  preload:
    - "otp-manager"

---

# Your Role

You are the **verification subagent** for erlmcp builds. Your job is to verify that the OTP environment is correctly set up and all tests pass.

## Verification Checklist

1. **OTP Version Check**
   \`\`\`bash
   erl -noshell -eval 'io:format("~s~n", [erlang:system_info(otp_release)])' -s init stop
   \`\`\`
   Must be >= 28.3.1

2. **Compilation Check**
   \`\`\`bash
   TERM=dumb rebar3 compile
   \`\`\`
   Must exit 0 (no errors)

3. **Unit Test Check**
   \`\`\`bash
   rebar3 eunit --application=erlmcp_core
   \`\`\`
   Must show all tests pass

## Return Format

If all checks pass:
\`\`\`json
{
  "ok": true,
  "checks": {
    "otp_version": "28.3.1",
    "compilation": "pass",
    "unit_tests": "pass"
  }
}
\`\`\`

If any check fails:
\`\`\`json
{
  "ok": false,
  "reason": "OTP version is 25.3.2, need 28.3.1+",
  "failed_check": "otp_version"
}
\`\`\`
```

---

## Part 4: How This Compares to My Original Approach

| Aspect | My Original (Work Orders) | Native Claude Code Web (Hooks + Skills + Subagents) |
|--------|--------------------------|-----------------------------------------------|
| **Orchestration** | Custom JSON queue system | Native hook lifecycle events |
| **Execution** | Agent picks from queue | Subagent launched by hook |
| **Verification** | Custom verifier module | Stop agent hook (built-in) |
| **Configuration** | Custom CLAUDE.md sections | Native settings.json (parsed by runtime) |
| **Distribution** | Custom plugin system | Native plugin/marketplace |
| **Audit Trail** | Custom receipt module | SessionEnd hook + transcript path |
| **Network Governance** | Custom domain checker | PreToolUse hook (native hook event) |
| **Code Reuse** | Work order dependencies | Native skills + subagents |

**Key wins of native approach**:
- ✅ Hooks are **actually invoked** by Claude Code runtime (not a custom layer)
- ✅ Subagents are **permission-scoped** at the product level
- ✅ SessionStart **persists env vars** for the entire session (native feature)
- ✅ Stop hook is **called before completion** (enforcement point built-in)
- ✅ Settings.json is **parsed by runtime** (not by agents)
- ✅ Plugins have **marketplace distribution** (not DIY)
- ✅ Transcript + session_id are **injected by runtime** (automatic receipts)

---

## Part 5: The "3-Layer" OTP Governance Implementation

**Layer 1: Sandbox + Network Policy** (Claude Code product)
- Network proxy enforces allowlist (GitHub, hex.pm, erlang-solutions.com)
- Filesystem isolation (session ephemeral)

**Layer 2: Hook-Based Runtime Governor** (.claude/hooks + .claude/settings.json)
```
SessionStart:     OTP bootstrap (idempotent, env vars → CLAUDE_ENV_FILE)
PreToolUse:       Block/ask/allow (network + FS safety)
PostToolUse:      Async CI triggering (compile + incremental tests)
Stop:             Agent-based verification (OTP version + tests + no regressions)
SessionEnd:       Receipt generation (audit trail)
```

**Layer 3: Skills + Subagents** (Reusable procedures + role-based execution)
```
/otp-manager:     Fetch, build, verify, clean OTP
verifier:         Read-only, Bash-only, verification specialization
build-engineer:   Write in src/, Bash, can edit code
release-scout:    WebSearch-only, find new Erlang releases
```

**Example: "Fix a bug" flow**
```
Developer: "Fix the race condition in erlmcp_session_manager"

1. SessionStart hook fires
   → OTP 28 installed (if missing)
   → Env vars set
   → Core pre-compiled

2. Agent takes control
   → Bash calls go through policy-bash.sh (PreToolUse)
   → Code edits trigger async CI (PostToolUse)
   → Reads go through sandbox (enforced)

3. Agent calls Stop
   → Stop hook fires
   → Spawns verifier subagent
   → Verifier runs: erl -noshell, rebar3 eunit, rebar3 compile
   → Returns {ok: true/false}
   → If true, agent stops; if false, blocking error

4. Session ends
   → SessionEnd hook fires
   → Receipt written with OTP version, build hash, test results
   → Transcript archived
```

---

## Part 6: Why This Becomes "Boring Infrastructure"

The goal of Armstrong's philosophy is to make **incorrect behavior impossible**. Native Claude Code Web primitives achieve this:

| Invariant | Enforcement |
|-----------|------------|
| **"OTP is always 28.3.1+"** | SessionStart idempotent, checked before compilation |
| **"Network only reaches GitHub/hex.pm"** | Sandbox proxy + PreToolUse hook (belt and suspenders) |
| **"Never stop without passing tests"** | Stop hook refuses to complete unless verifier returns ok=true |
| **"Audit trail always exists"** | SessionEnd hook + transcript_path (automatic) |
| **"Code edits trigger CI"** | PostToolUse async hook on Write/Edit |
| **"Subagents can't escalate permissions"** | toolAccess denylists enforced by product |

No custom layers, no agents remembering rules, no JSON files agents parse. **The runtime enforces it.**

---

## Part 7: Deployment Path

### Phase 1: Hooks Only (Weeks 1-2)
- Implement SessionStart.sh (OTP bootstrap)
- Implement policy-bash.sh (network governance)
- Test in a real cloud session

### Phase 2: Skills (Week 3)
- Package OTP operations as `/otp-manager` skill
- Preload into subagents

### Phase 3: Subagents (Week 4)
- Define verifier, build-engineer, release-scout
- Test subagent spawning from Stop hook

### Phase 4: Documentation (Week 5)
- Update CLAUDE.md to explain governance
- Create runbooks for each hook
- Add troubleshooting guide

---

## Conclusion

**The real innovation is**: Stop inventing control planes. Use Claude Code Web's **native primitives** (hooks, skills, subagents, settings scopes) as your **programmable runtime governor**.

Hooks are the **only** authorization points you need. They're called by the product. They return structured decisions. They have access to the session context (session_id, transcript_path, etc.).

This is boring infrastructure. It should be. It makes incorrect behavior unrepresentable by design.

---

**For questions**: Refer to Claude Code documentation on hooks, settings.json, subagents, and MCP.
**Next**: Implement Phase 1 (SessionStart + policy-bash hooks).
