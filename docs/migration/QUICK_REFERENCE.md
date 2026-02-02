# OTP Migration Quick Reference Guide

**Purpose**: Fast command reference for OTP migration tasks
**Scope**: Commands for all migration phases

---

## Table of Contents

1. [Pre-Migration Commands](#1-pre-migration-commands)
2. [Migration Commands](#2-migration-commands)
3. [Post-Migration Commands](#3-post-migration-commands)
4. [Rollback Commands](#4-rollback-commands)
5. [Monitoring Commands](#5-monitoring-commands)
6. [Troubleshooting Commands](#6-troubleshooting-commands)

---

## 1. Pre-Migration Commands

### Check Current State

```bash
# Check current OTP version
erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]).' -s init stop

# Check detailed version info
erl -noshell -eval '
    {Major, Minor, Patch} = erlang:system_info(otp_release),
    io:format("OTP: ~s.~s.~s~n", [Major, Minor, Patch]),
    halt(0)
.'

# Check available OTP versions
ls -la ~/.erlmcp/

# Check disk space
df -h .

# Check git status
git status --porcelain
```

### Create Backup

```bash
# Quick backup
BACKUP_DIR="backups/pre_migration_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$BACKUP_DIR"
cp -r config "$BACKUP_DIR/"
cp rebar.config "$BACKUP_DIR/"
cp vm.args "$BACKUP_DIR/"

# Verify backup
ls -la "$BACKUP_DIR"
```

### Run Baseline Tests

```bash
# Quick smoke test
rebar3 compile
erl -pa _build/default/lib/*/ebin -noshell -eval '
    {ok, _} = application:ensure_all_started(erlmcp),
    io:format("✅ OK~n"), halt(0).'

# Full test suite
rebar3 eunit
rebar3 ct

# Performance baseline
make benchmark-quick

# Quality gates
make check
```

---

## 2. Migration Commands

### Install OTP Version

```bash
# Using kerl (recommended)
kerl build 28.3.1 otp-28.3.1
kerl install otp-28.3.1 ~/.erlmcp/otp-28.3.1

# Activate OTP
source ~/.erlmcp/otp-28.3.1/activate

# Verify installation
erl -noshell -eval 'io:format("OTP ~s~n", [erlang:system_info(otp_release)]), halt(0).'
```

### Using Migration Script

```bash
# Automated migration
./scripts/migrate_otp_version.sh --target 28 --verbose

# Dry run
./scripts/migrate_otp_version.sh --target 28 --dry-run

# With test execution
./scripts/migrate_otp_version.sh --target 28 --run-tests
```

### Manual Migration Steps

```bash
# Step 1: Install OTP (see above)

# Step 2: Update rebar.config
sed -i.bak 's/{minimum_otp_vsn, "2[6-7]"}/{minimum_otp_vsn, "28"}/' rebar.config

# Step 3: Update environment
export ERLMCP_OTP_BIN="/Users/sac/.erlmcp/otp-28.3.1/bin"
export PATH="$ERLMCP_OTP_BIN:$PATH"

# Step 4: Clean and compile
rebar3 clean
rebar3 compile

# Step 5: Verify
erl -pa _build/default/lib/*/ebin -noshell -eval '
    {ok, _} = application:ensure_all_started(erlmcp),
    io:format("✅ Compiled and loaded~n"), halt(0).'
```

---

## 3. Post-Migration Commands

### Verification Commands

```bash
# Verify OTP version
erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]).' -s init stop

# Verify compilation
rebar3 compile

# Verify application starts
erl -pa _build/default/lib/*/ebin -noshell -eval '
    {ok, _} = application:ensure_all_started(erlmcp),
    {ok, Children} = supervisor:which_children(erlmcp_sup),
    io:format("✅ App started, ~p children~n", [length(Children)]),
    halt(0).'
```

### Test Commands

```bash
# Smoke tests
erl -pa _build/default/lib/*/ebin -noshell -eval '
    {ok, _} = application:ensure_all_started(erlmcp),
    ok = erlmcp_registry:register_server(test, self(), #{}),
    io:format("✅ Registry OK~n"), halt(0).'

# Unit tests
rebar3 eunit --verbose

# Integration tests
rebar3 ct --verbose

# Coverage
rebar3 cover
open _build/test/cover/index.html
```

### Performance Comparison

```bash
# Run benchmarks
make benchmark-quick > after.log

# Compare with baseline
python3 scripts/compare_benchmarks.py \
    --baseline backups/pre_migration_*/baseline.log \
    --current after.log \
    --threshold 0.1
```

---

## 4. Rollback Commands

### Automated Rollback

```bash
# Quick rollback
./scripts/rollback_otp.sh

# Rollback to specific backup
./scripts/rollback_otp.sh --backup backups/pre_migration_20260201_120000

# Rollback to specific OTP version
./scripts/rollback_otp.sh --otp-version 26.2.5
```

### Manual Rollback

```bash
# Step 1: Stop application
rebar3 release stop
killall beam.smp

# Step 2: Switch OTP version
source ~/.erlmcp/otp-26.2.5/activate

# Step 3: Restore configuration
cp -r backups/pre_migration_*/config ./

# Step 4: Recompile
rebar3 clean
rebar3 compile

# Step 5: Start application
rebar3 release start

# Step 6: Verify
./scripts/health_check.sh
```

### Git Rollback

```bash
# Reset code changes
git reset --hard HEAD~1

# Recompile
rebar3 clean
rebar3 compile

# Start
rebar3 release start
```

---

## 5. Monitoring Commands

### Health Checks

```bash
# Application health
curl http://localhost:8080/health

# Process count
erl -noshell -eval 'io:format("~p processes~n", [length(erlang:processes())]), halt(0).'

# Memory usage
erl -noshell -eval 'io:format("~p MB~n", [erlang:memory(total) div 1048576]), halt(0).'

# Supervisor status
erl -noshell -eval '
    {ok, Children} = supervisor:which_children(erlmcp_sup),
    lists:foreach(fun({Id, Pid, Type, Modules}) ->
        io:format("~p: ~p (~p)~n", [Id, Pid, Type])
    end, Children),
    halt(0).'
```

### Log Monitoring

```bash
# Real-time logs
tail -f log/erlang.log.1

# Error logs only
tail -f log/erlang.log.1 | grep -i error

# Recent errors
tail -100 log/erlang.log.1 | grep -i error

# Statistics
grep -c "error" log/erlang.log.1
grep -c "warning" log/erlang.log.1
```

### Performance Monitoring

```bash
# Throughput (if metrics enabled)
curl http://localhost:8080/metrics

# Observer (GUI)
erl -s observer start

# Quick benchmark
make benchmark-quick

# Continuous monitoring (24 hours)
watch -n 3600 './scripts/health_check.sh'
```

---

## 6. Troubleshooting Commands

### Compilation Issues

```bash
# Check OTP version
erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]).' -s init stop

# Check for errors
rebar3 compile 2>&1 | grep -i error

# Clean build
rebar3 clean
rebar3 delete-deps
rebar3 compile

# Check dependencies
rebar3 tree | grep -i error
```

### Test Failures

```bash
# Run single test
rebar3 eunit --module=erlmcp_registry_tests

# Run with verbose output
rebar3 eunit --verbose

# Check test logs
cat log/ct/run.*.log

# Run specific test case
rebar3 ct --suite=erlmcp_integration_SUITE --case=test_registry
```

### Performance Issues

```bash
# Compare with baseline
python3 scripts/compare_benchmarks.py \
    --current results.json \
    --baseline .erlmcp/baseline.json

# Check configuration
erl -noshell -eval '
    {ok, Limits} = application:get_env(erlmcp, resource_limits),
    io:format("~p~n", [Limits]),
    halt(0).'

# Check for optimizations
erl -noshell -eval '
    Enabled = erlang:system_info(otp_release) >= 28,
    io:format("OTP 28 optimizations: ~p~n", [Enabled]),
    halt(0).'
```

### Runtime Issues

```bash
# Check error logger
erl -noshell -eval '
    error_logger:info_msg("Test message~n"),
    halt(0).'

# Check process count
erl -noshell -eval '
    Procs = length(erlang:processes()),
    io:format("Processes: ~p~n", [Procs]),
    halt(0).'

# Check memory
erl -noshell -eval '
    Mem = erlang:memory(),
    io:format("Total: ~p MB~n", [proplists:get_value(total, Mem) div 1048576]),
    io:format("ETS: ~p MB~n", [proplists:get_value(ets, Mem) div 1048576]),
    halt(0).'

# Check port count
erl -noshell -eval '
    Ports = length(erlang:ports()),
    io:format("Ports: ~p~n", [Ports]),
    halt(0).'
```

---

## One-Liner Commands

### Quick Checks

```bash
# OTP version
erl -noshell -eval 'io:format("~s~n", [erlang:system_info(otp_release)]).' -s init stop

# Compilation status
rebar3 compile && echo "✅ OK" || echo "❌ FAIL"

# Application start
erl -pa _build/default/lib/*/ebin -noshell -eval '{ok,_}=application:ensure_all_started(erlmcp),halt(0).' && echo "✅ OK" || echo "❌ FAIL"

# Test status
rebar3 eunit >/dev/null 2>&1 && echo "✅ OK" || echo "❌ FAIL"

# Process count
erl -noshell -eval 'io:format("~p~n", [length(erlang:processes())]), halt(0).'

# Memory usage
erl -noshell -eval 'io:format("~p MB~n", [erlang:memory(total) div 1048576]), halt(0).'
```

### Quick Actions

```bash
# Full clean rebuild
rebar3 clean && rebar3 compile && echo "✅ Rebuilt"

# Run all tests
rebar3 eunit && rebar3 ct && echo "✅ All tests pass"

# Full quality check
make check && echo "✅ All gates pass"

# Quick backup
BACKUP="backups/quick_$(date +%Y%m%d_%H%M%S)" && mkdir -p "$BACKUP" && cp -r config rebar.config vm.args "$BACKUP/" && echo "✅ Backed up to $BACKUP"

# Quick health check
curl -sf http://localhost:8080/health && echo "✅ Healthy" || echo "❌ Unhealthy"
```

---

## Command Sequences

### Full Migration Workflow

```bash
# Complete migration (26 → 28)
./scripts/migrate_otp_version.sh --target 28 --verbose && \
rebar3 eunit && \
rebar3 ct && \
make benchmark-quick && \
echo "✅ Migration complete"
```

### Validation Workflow

```bash
# Complete validation
rebar3 compile && \
erl -pa _build/default/lib/*/ebin -noshell -eval '{ok,_}=application:ensure_all_started(erlmcp),halt(0).' && \
rebar3 eunit && \
rebar3 ct && \
make check && \
echo "✅ All validations pass"
```

### Monitoring Workflow

```bash
# Continuous monitoring (24 hours)
for i in {1..24}; do
    echo "Hour $i:"
    ./scripts/health_check.sh
    sleep 3600
done
```

---

## Common Patterns

### Check Something, Exit if Fail

```bash
# Pattern: command || exit 1
rebar3 compile || exit 1

# Pattern: if command; then ...; fi
if rebar3 eunit; then
    echo "Tests pass"
else
    echo "Tests fail" && exit 1
fi
```

### Run Command with Timeout

```bash
# Pattern: timeout 30s command
timeout 30s rebar3 compile

# Pattern: command & sleep 10; kill $!
rebar3 compile &
PID=$!
sleep 10
kill $PID 2>/dev/null || true
```

### Retry Pattern

```bash
# Pattern: for i in {1..3}; do command && break || sleep 5; done
for i in {1..3}; do
    rebar3 compile && break || sleep 5
done
```

---

## Aliases (Add to ~/.bashrc or ~/.zshrc)

```bash
# OTP migration aliases
alias otp-version='erl -noshell -eval '\''io:format("~s~n", [erlang:system_info(otp_release)]), halt(0).'\'
alias otp-compile='rebar3 clean && rebar3 compile'
alias otp-test='rebar3 eunit && rebar3 ct'
alias otp-health='curl -sf http://localhost:8080/health || echo "Unhealthy"'
alias otp-mem='erl -noshell -eval '\''io:format("~p MB~n", [erlang:memory(total) div 1048576]), halt(0).'\'
alias otp-procs='erl -noshell -eval '\''io:format("~p processes~n", [length(erlang:processes())]), halt(0).'\'

# Quick backup
alias otp-backup='BACKUP="backups/quick_$(date +%Y%m%d_%H%M%S)" && mkdir -p "$BACKUP" && cp -r config rebar.config vm.args "$BACKUP/" && echo "Backed up to $BACKUP"'

# Full check
alias otp-check='rebar3 compile && rebar3 eunit && rebar3 ct && make check && echo "✅ All checks pass"'
```

---

## Script Templates

### Pre-Migration Script

```bash
#!/bin/bash
# Quick pre-migration check

echo "=== Pre-Migration Check ==="

# OTP version
OTP=$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]).' -s init stop)
echo "Current OTP: $OTP"

# Git status
if [ -z "$(git status --porcelain)" ]; then
    echo "✅ Git clean"
else
    echo "❌ Git not clean"
    exit 1
fi

# Compile
if rebar3 compile >/dev/null 2>&1; then
    echo "✅ Compiles"
else
    echo "❌ Compile fails"
    exit 1
fi

# Backup
BACKUP="backups/pre_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$BACKUP"
cp -r config rebar.config vm.args "$BACKUP/"
echo "✅ Backed up to $BACKUP"

echo "✅ Ready for migration"
```

### Post-Migration Script

```bash
#!/bin/bash
# Quick post-migration validation

echo "=== Post-Migration Validation ==="

# OTP version
OTP=$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]).' -s init stop)
echo "Current OTP: $OTP"

# Compile
rebar3 compile || exit 1

# Start test
erl -pa _build/default/lib/*/ebin -noshell -eval '
    {ok, _} = application:ensure_all_started(erlmcp),
    io:format("✅ App starts~n"),
    halt(0).' || exit 1

# Tests
rebar3 eunit || exit 1

echo "✅ Validation complete"
```

---

## Tips and Tricks

### Efficiency Tips

1. **Use parallel execution** for independent tasks:
   ```bash
   rebar3 eunit &  # Run in background
   EUNIT_PID=$!
   rebar3 ct &     # Run in background
   CT_PID=$!
   wait $EUNIT_PID $CT_PID  # Wait for both
   ```

2. **Use make targets** for common workflows:
   ```bash
   make check      # All quality gates
   make test       # All tests
   make benchmark-quick  # Quick benchmarks
   ```

3. **Cache compilation** for faster iterations:
   ```bash
   # Don't clean if just changing source
   rebar3 compile  # Faster than clean + compile
   ```

### Safety Tips

1. **Always backup** before major changes:
   ```bash
   otp-backup  # Use alias
   ```

2. **Test in staging** before production:
   ```bash
   # Deploy to staging first
   ./scripts/deploy.sh staging
   ./scripts/validate.sh staging
   ```

3. **Use dry-run** mode:
   ```bash
   ./scripts/migrate_otp_version.sh --dry-run
   ```

### Debugging Tips

1. **Enable verbose output**:
   ```bash
   rebar3 compile --verbose
   rebar3 eunit --verbose
   ```

2. **Check individual components**:
   ```bash
   rebar3 compile --app erlmcp_core
   rebar3 eunit --module=erlmcp_registry_tests
   ```

3. **Use Erlang shell** for debugging:
   ```bash
   erl -pa _build/default/lib/*/ebin
   > application:ensure_all_started(erlmcp).
   > erlmcp_registry:register_server(test, self(), #{}).
   ```

---

## Emergency Commands

### Stop Everything

```bash
# Stop application
rebar3 release stop

# Kill all BEAM processes
killall beam.smp

# Kill all Erlang processes
pkill -9 beam

# Force stop
killall -9 beam.smp
```

### Emergency Rollback

```bash
# Quick rollback (assuming backup exists)
LATEST_BACKUP=$(ls -t backups/pre_migration_* | head -1)
cp -r "$LATEST_BACKUP/config" ./
source ~/.erlmcp/otp-26.2.5/activate
rebar3 clean
rebar3 compile
rebar3 release start
```

### Emergency Health Check

```bash
# Is the app running?
pgrep -f beam.smp

# Is the port listening?
lsof -i :8080

# Can we make a request?
curl -v http://localhost:8080/health

# What OTP is running?
pgrep -a beam | grep erl
```

---

## Reference Cards

### Pre-Migration (Print This)

```
┌─────────────────────────────────────┐
│     PRE-MIGRATION CHECKLIST         │
├─────────────────────────────────────┤
│ ☐ Disk space >= 5GB                 │
│ ☐ Git working directory clean       │
│ ☐ Backup created                    │
│ ☐ Baseline tests pass               │
│ ☐ Stakeholders notified             │
└─────────────────────────────────────┘

COMMANDS:
  otp-version          # Check OTP
  git status           # Check git
  otp-backup           # Quick backup
  otp-check            # Full check
```

### Post-Migration (Print This)

```
┌─────────────────────────────────────┐
│    POST-MIGRATION VALIDATION        │
├─────────────────────────────────────┤
│ ☐ OTP version verified              │
│ ☐ Application starts                │
│ ☐ All tests pass                    │
│ ☐ Performance validated             │
│ ☐ 24h stable                        │
└─────────────────────────────────────┘

COMMANDS:
  otp-version          # Verify OTP
  otp-compile          # Compile
  otp-test             # Run tests
  otp-health           # Health check
```

---

**END OF QUICK REFERENCE**
