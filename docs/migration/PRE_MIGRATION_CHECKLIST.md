# Pre-Migration Checklist

**Migration**: OTP [FROM_VERSION] → OTP [TO_VERSION]
**Date**: [YYYY-MM-DD]
**Owner**: [NAME]
**Maintenance Window**: [START] - [END]

---

## Section 1: Prerequisites (30 minutes)

### Infrastructure Readiness
- [ ] **Disk Space**: Verify >= 5GB free
  ```bash
  df -h . | tail -1
  ```
  Expected: >= 5GB available

- [ ] **Memory**: Verify >= 4GB RAM available
  ```bash
  free -h  # Linux
  vm_stat  # macOS
  ```

- [ ] **Network**: Verify outbound internet access (for downloading OTP)
  ```bash
  curl -I https://github.com
  ```

- [ ] **Git Status**: Working directory must be clean
  ```bash
  git status --porcelain
  ```
  Expected: No output

### Tool Installation
- [ ] **kerl**: Installed and in PATH
  ```bash
  kerl version
  ```
  Expected: kerl version X.X.X

- [ ] **rebar3**: Installed and working
  ```bash
  rebar3 version
  ```
  Expected: rebar3 X.X.X

- [ ] **Python 3**: For benchmark comparison scripts
  ```bash
  python3 --version
  ```
  Expected: Python 3.8+

### Access Verification
- [ ] **Write access** to installation directory (`/Users/sac/.erlmcp/`)
- [ ] **Write access** to project directory (`/Users/sac/erlmcp/`)
- [ ] **Admin/sudo access** (if installing system-wide OTP)

---

## Section 2: Backup and Baseline (30 minutes)

### Configuration Backup
- [ ] **Backup configuration files**
  ```bash
  BACKUP_DIR="backups/pre_migration_$(date +%Y%m%d_%H%M%S)"
  mkdir -p "$BACKUP_DIR"
  cp -r config "$BACKUP_DIR/"
  cp rebar.config "$BACKUP_DIR/"
  cp vm.args "$BACKUP_DIR/"
  ```

- [ ] **Verify backup created**
  ```bash
  ls -la "$BACKUP_DIR"
  ```
  Expected: config/, rebar.config, vm.args

- [ ] **Record backup location**
  ```
  Backup ID: pre_migration_YYYYMMDD_HHMMSS
  Location: /Users/sac/erlmcp/backups/pre_migration_YYYYMMDD_HHMMSS
  ```

### Data Backup
- [ ] **Backup ETS tables** (if using)
  ```bash
  # Run from Erlang shell
  erl -noshell -eval '
      ets:tab2list(session_table),
      ets:tab2list(registry_table),
      halt(0)
  ' > "$BACKUP_DIR/ets_data.txt"
  ```

- [ ] **Backup Mnesia data** (if using)
  ```bash
  mnesia:backup("$BACKUP_DIR/mnesia_backup").
  ```

### Baseline Performance
- [ ] **Run baseline benchmarks**
  ```bash
  make benchmark-quick > "$BACKUP_DIR/baseline_before.log" 2>&1
  ```

- [ ] **Record current OTP version**
  ```bash
  CURRENT_OTP=$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]).' -s init stop)
  echo "Current OTP: $CURRENT_OTP" > "$BACKUP_DIR/otp_version.txt"
  ```

- [ ] **Record git commit**
  ```bash
  git rev-parse HEAD > "$BACKUP_DIR/git_commit.txt"
  git log -1 --pretty=format:"%H %s %an" >> "$BACKUP_DIR/git_commit.txt"
  ```

### Baseline Tests
- [ ] **Run EUnit tests**
  ```bash
  rebar3 eunit --verbose 2>&1 | tee "$BACKUP_DIR/eunit_before.log"
  ```
  Expected: All tests pass

- [ ] **Run CT tests**
  ```bash
  rebar3 ct --verbose 2>&1 | tee "$BACKUP_DIR/ct_before.log"
  ```
  Expected: All tests pass

- [ ] **Record test coverage**
  ```bash
  rebar3 cover
  cp _build/test/cover/*.html "$BACKUP_DIR/"
  ```

---

## Section 3: Validation (1 hour)

### Pre-Migration Health Check
- [ ] **Application starts successfully**
  ```bash
  erl -pa _build/default/lib/*/ebin -noshell -eval '
      {ok, _} = application:ensure_all_started(erlmcp),
      io:format("✅ Application started~n"),
      halt(0)
  '
  ```

- [ ] **Supervision tree healthy**
  ```bash
  erl -pa _build/default/lib/*/ebin -noshell -eval '
      {ok, Children} = supervisor:which_children(erlmcp_sup),
      io:format("✅ Supervisor children: ~p~n", [length(Children)]),
      halt(0)
  '
  ```

- [ ] **Registry operations working**
  ```bash
  erl -pa _build/default/lib/*/ebin -noshell -eval '
      {ok, _} = application:ensure_all_started(erlmcp),
      ok = erlmcp_registry:register_server(test_server, self(), #{}),
      {ok, Pid} = erlmcp_registry:find_server(test_server),
      io:format("✅ Registry working~n"),
      halt(0)
  '
  ```

### Quality Gate Verification
- [ ] **Compile**: 0 errors
  ```bash
  rebar3 compile 2>&1 | grep -i error
  ```
  Expected: No output

- [ ] **Dialyzer**: 0 warnings
  ```bash
  rebar3 dialyzer 2>&1 | grep -i warning
  ```
  Expected: No warnings (or known safe warnings)

- [ ] **Xref**: 0 undefined functions
  ```bash
  rebar3 xref 2>&1 | grep "undefined"
  ```
  Expected: No undefined functions

### Dependency Check
- [ ] **Verify all dependencies compatible with target OTP**
  ```bash
  rebar3 tree | grep -E "(gun|ranch|cowboy|gproc|jsx)"
  ```
  Expected: Latest versions shown

- [ ] **Check for deprecated dependencies**
  ```bash
  rebar3 tree | grep -i deprecated
  ```
  Expected: No output

---

## Section 4: Stakeholder Communication (30 minutes)

### Notification
- [ ] **Email notification sent** to:
  - [ ] Development team
  - [ ] Operations team
  - [ ] Product management
  - [ ] Stakeholders

**Email Template**:
```
Subject: [ACTION REQUIRED] OTP Migration - [DATE]

Hi Team,

We will be upgrading Erlang/OTP from [X] to [Y] on [DATE].

**Maintenance Window**: [START] - [END] ([DURATION] hours)
**Expected Impact**: [No downtime / Downtime expected]
**Services Affected**: erlmcp services

**Migration Steps**:
1. Pre-migration backup and validation
2. OTP installation
3. Compilation and testing
4. Production deployment
5. 24-hour monitoring

**Rollback Plan**: Immediate rollback if issues detected

**Questions? Contact**: [YOUR NAME]

Migration Plan: [LINK TO DOCUMENTATION]
```

### Approval
- [ ] **Maintenance window approved** by:
  - [ ] Engineering Manager
  - [ ] Product Manager
  - [ ] Operations Lead

- [ ] **Change ticket created**
  ```
  Ticket ID: [CHANGE-XXX]
  Link: [URL]
  ```

---

## Section 5: Rollback Preparation (30 minutes)

### Rollback Plan Verification
- [ ] **Rollback script tested**
  ```bash
  ./scripts/rollback_otp.sh --dry-run
  ```
  Expected: No errors

- [ ] **Previous OTP installation verified**
  ```bash
  ls -la /Users/sac/.erlmcp/otp-[OLD_VERSION]/
  ```
  Expected: Directory exists

- [ ] **Rollback procedure documented**
  - [ ] Steps documented in runbook
  - [ ] Escalation contacts updated
  - [ ] Rollback decision criteria defined

### Rollback Triggers Confirmed
- [ ] **Automatic rollback triggers defined**:
  - Error rate > 5%
  - Latency > 3x baseline
  - Service availability < 99%
  - Data corruption detected

- [ ] **Manual rollback triggers defined**:
  - Feature not working
  - Customer complaints
  - Business impact

---

## Section 6: Final Go/No-Go (15 minutes)

### Go/No-Go Checklist

| Category | Item | Status | Notes |
|----------|------|--------|-------|
| **Infrastructure** | Disk space >= 5GB | ☐ Yes / ☐ No | |
| **Infrastructure** | Memory >= 4GB | ☐ Yes / ☐ No | |
| **Infrastructure** | Network access | ☐ Yes / ☐ No | |
| **Tools** | kerl installed | ☐ Yes / ☐ No | |
| **Tools** | rebar3 installed | ☐ Yes / ☐ No | |
| **Backup** | Config backed up | ☐ Yes / ☐ No | Location: ____ |
| **Backup** | Data backed up | ☐ Yes / ☐ No | Location: ____ |
| **Baseline** | Tests pass | ☐ Yes / ☐ No | |
| **Baseline** | Performance measured | ☐ Yes / ☐ No | |
| **Validation** | Quality gates pass | ☐ Yes / ☐ No | |
| **Validation** | Dependencies OK | ☐ Yes / ☐ No | |
| **Communication** | Stakeholders notified | ☐ Yes / ☐ No | |
| **Approval** | Change ticket approved | ☐ Yes / ☐ No | |
| **Rollback** | Rollback plan ready | ☐ Yes / ☐ No | |
| **Rollback** | Previous OTP available | ☐ Yes / ☐ No | |

### Final Decision

**ALL items must be "Yes" before proceeding with migration.**

- [ ] **GO**: Proceed with migration
- [ ] **NO-GO**: Do not proceed - document blockers

**Signed**: _______________________  **Date**: _____________

**Approved By**: ___________________  **Date**: _____________

---

## Section 7: Pre-Migration Script

Run this automated script to complete all checks:

```bash
#!/bin/bash
# scripts/pre_migration_check.sh

set -e

echo "=== Pre-Migration Checklist ==="
echo ""

# Variables
BACKUP_DIR="backups/pre_migration_$(date +%Y%m%d_%H%M%S)"
CURRENT_OTP=$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]).' -s init stop)

# Section 1: Prerequisites
echo "Section 1: Prerequisites"
echo "------------------------"

# Disk space
DISK_FREE=$(df -h . | tail -1 | awk '{print $4}')
DISK_FREE_GB=$(df -BG . | tail -1 | awk '{print $4}' | tr -d 'G')
if [ "$DISK_FREE_GB" -ge 5 ]; then
    echo "✅ Disk space: $DISK_FREE (>= 5GB)"
else
    echo "❌ Disk space: $DISK_FREE (< 5GB) - BLOCKER"
    exit 1
fi

# Git status
if [ -z "$(git status --porcelain)" ]; then
    echo "✅ Git working directory clean"
else
    echo "❌ Git working directory not clean - BLOCKER"
    git status --short
    exit 1
fi

# Tools
if command -v kerl >/dev/null 2>&1; then
    echo "✅ kerl installed: $(kerl version)"
else
    echo "❌ kerl not installed - BLOCKER"
    exit 1
fi

if command -v rebar3 >/dev/null 2>&1; then
    echo "✅ rebar3 installed: $(rebar3 version)"
else
    echo "❌ rebar3 not installed - BLOCKER"
    exit 1
fi

echo ""

# Section 2: Backup
echo "Section 2: Backup and Baseline"
echo "-------------------------------"

mkdir -p "$BACKUP_DIR"
cp -r config "$BACKUP_DIR/" 2>/dev/null || true
cp rebar.config "$BACKUP_DIR/" 2>/dev/null || true
cp vm.args "$BACKUP_DIR/" 2>/dev/null || true

echo "✅ Backup created: $BACKUP_DIR"

# Record OTP version
echo "Current OTP: $CURRENT_OTP" > "$BACKUP_DIR/otp_version.txt"
echo "✅ OTP version recorded"

# Record git commit
git rev-parse HEAD > "$BACKUP_DIR/git_commit.txt"
echo "✅ Git commit recorded"

echo ""

# Section 3: Validation
echo "Section 3: Validation"
echo "--------------------"

# Compile
if rebar3 compile >/dev/null 2>&1; then
    echo "✅ Compilation successful"
else
    echo "❌ Compilation failed - BLOCKER"
    exit 1
fi

# Tests (optional - can be long)
read -p "Run full test suite? (y/N): " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    if rebar3 eunit >/dev/null 2>&1; then
        echo "✅ EUnit tests pass"
    else
        echo "❌ EUnit tests failed - BLOCKER"
        exit 1
    fi
fi

echo ""

# Section 4: Summary
echo "=== Pre-Migration Summary ==="
echo "Current OTP: $CURRENT_OTP"
echo "Backup location: $BACKUP_DIR"
echo "Git commit: $(cat $BACKUP_DIR/git_commit.txt)"
echo ""
echo "✅ All pre-migration checks passed"
echo ""
echo "Ready to proceed with migration."
echo "Run: ./scripts/migrate_otp_version.sh --target [VERSION]"
```

---

## Notes

**Common Issues**:

1. **Disk space full**: Clean up old backups
   ```bash
   du -sh backups/* | sort -h | tail -5
   rm -rf backups/old_migration_*
   ```

2. **Git working directory not clean**: Stash or commit changes
   ```bash
   git stash save "Pre-migration stash"
   ```

3. **kerl not found**: Install kerl
   ```bash
   curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl
   chmod +x kerl
   sudo mv kerl /usr/local/bin/
   ```

4. **Tests fail**: Fix tests before migration
   ```bash
   rebar3 eunit --verbose  # See which tests fail
   ```

**Estimated Time**: 2-3 hours

**Contact**: [YOUR NAME] for questions

---

**END OF CHECKLIST**
