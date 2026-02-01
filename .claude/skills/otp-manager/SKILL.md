---
name: otp-manager
version: 1.0.0
description: OTP 28.3.1 management skill for Erlang/OTP installation, verification, and cleanup
author: erlmcp
commands:
  - name: fetch-build
    description: Download and build OTP 28.3.1 from official sources
    usage: /otp-manager fetch-build
  - name: verify
    description: Check OTP version and compilation readiness
    usage: /otp-manager verify
  - name: clean
    description: Remove OTP build artifacts and cached files
    usage: /otp-manager clean
preload:
  - verifier
  - build-engineer
tags:
  - otp
  - erlang
  - build
  - infrastructure
---

# OTP Manager Skill

Reusable skill for managing Erlang/OTP 28.3.1 in Claude Code Web environments.

## Purpose

Provides automated procedures for:
- **OTP Installation**: Download and build OTP 28.3.1 from Erlang Solutions
- **OTP Verification**: Check installed version and compilation readiness
- **OTP Cleanup**: Remove build artifacts and cached files

## Commands

### `/otp-manager fetch-build`

Download and build OTP 28.3.1 from official Erlang Solutions packages.

**Behavior**:
1. Check if OTP 28.3.1 is already installed (idempotent)
2. Download erlang-solutions repository package
3. Install OTP 28.3.1 via apt-get
4. Verify installation success
5. Return status + version

**Idempotency**: Safe to run multiple times. Skips installation if OTP >= 28.3.1 already exists.

**Error Handling**:
- Network timeout: Retries 3 times with exponential backoff
- Package installation failure: Clears cache and retries
- Permission denied: Reports error (requires sudo)
- Version mismatch: Reports installed version vs required

**Output**:
```
✅ OTP 28.3.1 installed successfully
   Version: 28.3.1
   Location: /usr/lib/erlang
   Time: 45s
```

**Example**:
```bash
/otp-manager fetch-build
# Downloads and installs OTP 28.3.1
# Returns: exit code 0 (success) | 1 (failure)
```

---

### `/otp-manager verify`

Check OTP version and verify compilation readiness.

**IMPORTANT**: Always source the environment file first to ensure OTP 28 is used:
```bash
source .erlmcp/env.sh
```

**Behavior**:
1. Source `.erlmcp/env.sh` (ensures OTP 28.3.1 in PATH)
2. Detect installed OTP version (erl -noshell -eval)
3. Compare against required version (28.3.1)
4. Test basic compilation (rebar3 version)
5. Return verification result

**Checks Performed**:
- OTP version >= 28.3.1
- erl command available (from .erlmcp/env.sh)
- rebar3 command available
- Basic compilation works

**Output**:
```
✅ OTP verification passed
   Installed: 28.3.1
   Required:  28.3.1
   rebar3:    3.24.0
   Status:    READY
```

**Exit Codes**:
- 0: Verification passed (OTP >= 28.3.1)
- 1: OTP not found or version too low
- 2: OTP found but rebar3 missing
- 3: OTP found but compilation test failed

**Example**:
```bash
/otp-manager verify
# Sources .erlmcp/env.sh, checks OTP version and compilation readiness
# Returns: exit code 0-3 (see above)
```

---

### `/otp-manager clean`

Remove OTP build artifacts and cached files.

**Behavior**:
1. Remove build artifacts in `/tmp/erlmcp-build/`
2. Clear apt-get cache for erlang packages
3. Remove lock files in `.erlmcp/cache/`
4. Preserve installed OTP binaries (safe cleanup)

**Safety**:
- Does NOT uninstall OTP (only removes build artifacts)
- Does NOT remove user code or dependencies
- Does NOT affect rebar3 project builds

**Output**:
```
✅ OTP cleanup complete
   Removed: 123 MB build artifacts
   Cleared: 45 MB apt cache
   Preserved: OTP 28.3.1 installation
```

**Example**:
```bash
/otp-manager clean
# Removes build artifacts and cache
# Returns: exit code 0 (success)
```

---

## Integration with Subagents

This skill can be preloaded into subagents for autonomous OTP management:

**Verifier Subagent** (`.claude/agents/verifier.md`):
```yaml
preload_skills:
  - otp-manager
```

**Build Engineer Subagent** (`.claude/agents/build-engineer.md`):
```yaml
preload_skills:
  - otp-manager
```

Once preloaded, subagents can invoke OTP manager commands autonomously:

```bash
# Verifier checks OTP before running tests
/otp-manager verify
if [ $? -eq 0 ]; then
  rebar3 eunit
fi

# Build engineer fetches OTP if missing
/otp-manager verify || /otp-manager fetch-build
```

---

## Error Recovery

The skill implements robust error recovery:

| Error Class | Recovery Strategy |
|-------------|-------------------|
| Network timeout | Retry 3x with exponential backoff (1s, 2s, 4s) |
| Package conflict | Clear apt cache and retry |
| Permission denied | Report to user (requires sudo) |
| Version mismatch | Report installed vs required version |
| Disk space | Report error (requires user intervention) |

**Example: Network Timeout Recovery**
```
⚠️  Network timeout downloading erlang-solutions package
🔄 Retry 1/3 (delay: 1s)...
⚠️  Network timeout downloading erlang-solutions package
🔄 Retry 2/3 (delay: 2s)...
✅ Download successful
```

---

## Implementation Details

### Source Files

1. **otp_fetch_build.sh** (120 lines):
   - Downloads OTP 28.3.1 from erlang-solutions.com
   - Installs via apt-get with retry logic
   - Verifies installation success
   - Creates lock file for idempotency

2. **otp_verify.sh** (50 lines):
   - Detects OTP version via erl command
   - Compares against required version
   - Tests rebar3 availability
   - Returns structured exit codes

3. **otp_clean.sh** (30 lines):
   - Removes build artifacts
   - Clears apt cache
   - Preserves installed OTP

### Design Principles

- **Idempotent**: All commands safe to run multiple times
- **Fail-Safe**: Clear error messages, no silent failures
- **Cloud-Native**: Works in ephemeral VM environments
- **Reusable**: Can be invoked by multiple subagents
- **Auditable**: Logs all operations to `.erlmcp/otp-manager.log`

---

## Testing

Test suite location: `test/otp_manager_skill_tests.erl`

**Test Coverage**:
- ✅ Idempotency: fetch-build twice produces same result
- ✅ Version detection: verify detects OTP 28.3.1 correctly
- ✅ Error handling: Network timeout triggers retry
- ✅ Clean operation: Removes artifacts without breaking OTP
- ✅ Slash command: /otp-manager invocation works
- ✅ Subagent integration: Can be preloaded into verifier

**Running Tests**:
```bash
rebar3 eunit --module=otp_manager_skill_tests
```

Expected output:
```
Testing otp_manager_skill_tests
  All 6 tests passed.
```

---

## Related Files

- `.claude/hooks/SessionStart.sh` - Uses fetch-build logic on session start
- `.claude/agents/verifier.md` - Preloads this skill for OTP verification
- `.claude/agents/build-engineer.md` - Preloads this skill for OTP management
- `.claude/settings.json` - References otp-manager in skill registry

---

## Changelog

### v1.0.0 (2026-02-01)
- Initial implementation
- Commands: fetch-build, verify, clean
- Integration with verifier and build-engineer subagents
- Test coverage: 6 test cases
- Spec: AUTONOMOUS_IMPLEMENTATION_WORK_ORDER.md:WO-006
