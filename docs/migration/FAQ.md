# OTP Migration FAQ

**Last Updated**: 2026-02-01
**Maintainer**: [YOUR NAME]

---

## Table of Contents

1. [General Questions](#general-questions)
2. [Pre-Migration Questions](#pre-migration-questions)
3. [Migration Questions](#migration-questions)
4. [Post-Migration Questions](#post-migration-questions)
5. [Troubleshooting Questions](#troubleshooting-questions)
6. [Advanced Questions](#advanced-questions)

---

## General Questions

### Q1: Why should I upgrade OTP?

**A**: Key benefits of upgrading to OTP 28.3.1:

- **Performance**: 15-25% faster TLS, 3-4x faster Base64, O(1) process enumeration
- **Features**: Priority messages, process iterator, PCRE2 regex, native JSON
- **Security**: SSL verify_peer default, legacy algorithms disabled
- **Memory**: 90% reduction in idle supervisor memory with hibernation
- **Stability**: Bug fixes, improved monitoring, better error handling

**ROI**: Performance improvements typically justify migration effort within 1-2 months.

---

### Q2: What are the risks of upgrading?

**A**: Main risks and their mitigations:

| Risk | Probability | Impact | Mitigation |
|------|------------|--------|------------|
| Compilation failures | 30-40% | High | Pre-compile in staging |
| Test failures | 30-40% | High | Fix tests before migration |
| Performance regression | 10-20% | High | Benchmark before/after |
| Downtime | 15-20% | High | Blue-green deployment |
| Dependency conflicts | 10-15% | Medium | Dependency audit |

**Overall Risk Level**: 🟡 Medium (manageable with proper planning)

---

### Q3: How long does migration take?

**A**: Timeline depends on migration path:

- **OTP 26 → 27**: 2-3 hours (installation + compilation + tests)
- **OTP 27 → 28**: 2-3 hours (same as above)
- **OTP 26 → 28 (direct)**: 4-5 hours (larger jump)
- **Full migration with 24h monitoring**: 2-3 days

**Calendar Time**: Plan for 2 weeks including:
- Preparation: 1-2 days
- Migration: 1 day
- 24h monitoring: 1 day
- Documentation: 1-2 days

---

### Q4: Can I skip intermediate versions?

**A**: Yes, you can upgrade directly from OTP 26 to OTP 28.

**Pros**:
- Faster overall migration
- Fewer migration events
- Get all features immediately

**Cons**:
- Higher risk (bigger jump)
- Harder to isolate issues
- Less incremental validation

**Recommendation**: Use conservative path (26 → 27 → 28) for production systems.

---

### Q5: Do I need to upgrade dependencies?

**A**: Usually not, but verify compatibility:

```bash
# Check dependency versions
rebar3 tree | grep -E "(gun|ranch|cowboy|gproc)"

# Current compatible versions:
- gun: 2.0.1+ (HTTP/2 support)
- ranch: 2.1.0+ (TCP acceptor pool)
- cowboy: 2.10.0+ (HTTP server)
- gproc: 0.9.0+ (Process registry)
```

Most erlmcp dependencies are already compatible with OTP 28.

---

## Pre-Migration Questions

### Q6: What should I do before migrating?

**A**: Follow the pre-migration checklist:

1. **Backup everything**
   ```bash
   BACKUP_DIR="backups/pre_migration_$(date +%Y%m%d_%H%M%S)"
   mkdir -p "$BACKUP_DIR"
   cp -r config rebar.config vm.args "$BACKUP_DIR/"
   ```

2. **Run baseline tests**
   ```bash
   make benchmark-quick
   rebar3 eunit
   rebar3 ct
   ```

3. **Verify clean state**
   ```bash
   git status --porcelain  # Should be empty
   ```

4. **Notify stakeholders**
   - Send migration announcement
   - Schedule maintenance window
   - Get approval for change

---

### Q7: How do I know if I'm ready to migrate?

**A**: You're ready if ALL of these are true:

- ✅ Disk space >= 5GB free
- ✅ Git working directory clean
- ✅ All tests pass (EUnit + CT)
- ✅ Baseline performance measured
- ✅ Backup created and verified
- ✅ Stakeholders notified
- ✅ Maintenance window approved
- ✅ Rollback plan ready

**Use the automated check**:
```bash
./scripts/pre_migration_check.sh
```

---

### Q8: What if tests fail before migration?

**A**: Fix tests before migrating.

**Common issues**:
- Timing-dependent tests: Adjust timeouts
- OTP-specific behaviors: Use version guards
- Mock failures: Update mock expectations

**Don't migrate with failing tests** - it will be harder to debug post-migration.

---

## Migration Questions

### Q9: Should I use the automated script or manual migration?

**A**: Use the automated script for most cases.

**Automated script** (`scripts/migrate_otp_version.sh`):
- ✅ Faster
- ✅ Fewer errors
- ✅ Built-in validation
- ✅ Generates reports

**Manual migration**:
- More control
- Better understanding of process
- Useful for learning or custom setups

**Recommendation**: Use automated script for production, manual for development/testing.

---

### Q10: What if compilation fails during migration?

**A**: Follow these steps:

1. **Check OTP version**
   ```bash
   erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]).' -s init stop
   ```
   Expected: Target version (e.g., 28)

2. **Check for errors**
   ```bash
   rebar3 compile 2>&1 | grep -i error
   ```

3. **Clean and retry**
   ```bash
   rebar3 clean
   rebar3 delete-deps
   rebar3 compile
   ```

4. **Check dependencies**
   ```bash
   rebar3 tree | grep -i error
   ```

5. **Fix specific issues**
   - Update incompatible dependencies
   - Fix compilation errors in source
   - Add missing includes

---

### Q11: Can I migrate without downtime?

**A**: Yes, with blue-green deployment.

**Strategy**:
1. Deploy OTP 28 to separate environment (green)
2. Validate green environment
3. Switch traffic from blue (OTP 26) to green
4. Monitor green environment
5. Roll back if issues detected

**Tools**:
- Load balancer for traffic switching
- Health checks for validation
- Automated rollback on failure

---

### Q12: What if I run out of disk space during migration?

**A**: Free up space before migrating.

**Check disk usage**:
```bash
df -h .
du -sh _build/* | sort -h
```

**Clean up**:
```bash
# Remove old builds
rebar3 clean

# Remove old backups (keep recent 3)
ls -t backups/ | tail -n +4 | xargs rm -rf

# Remove test logs
rm -rf log/ct/*
rm -rf log/eunit/*
```

---

## Post-Migration Questions

### Q13: How do I verify the migration was successful?

**A**: Run the post-migration validation:

1. **Immediate validation** (30 minutes)
   ```bash
   # OTP version
   erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]).' -s init stop

   # Application starts
   erl -pa _build/default/lib/*/ebin -noshell -eval '{ok,_}=application:ensure_all_started(erlmcp),halt(0).'
   ```

2. **Smoke tests** (1 hour)
   ```bash
   rebar3 eunit --module=erlmcp_registry_tests
   ```

3. **Full tests** (2-4 hours)
   ```bash
   rebar3 eunit
   rebar3 ct
   ```

4. **Performance validation** (6 hours)
   ```bash
   make benchmark-quick
   ```

5. **24-hour monitoring**
   - Check error logs hourly
   - Monitor memory usage
   - Track performance metrics

---

### Q14: What if performance is worse after migration?

**A**: Common causes and fixes:

**Cause 1: OTP 28 optimizations not enabled**
```bash
# Check configuration
erl -noshell -eval '
    {ok, Features} = application:get_env(erlmcp, features),
    io:format("~p~n", [Features]),
    halt(0).'
```
**Fix**: Enable OTP 28 features in config

**Cause 2: Resource limits too low**
```bash
# Check limits
erl -noshell -eval '
    {ok, Limits} = application:get_env(erlmcp, resource_limits),
    io:format("~p~n", [Limits]),
    halt(0).'
```
**Fix**: Increase limits (processes, memory)

**Cause 3: Configuration not optimized for OTP 28**
**Fix**: Use OTP 28-specific configuration

**Acceptable regression**: < 10% (within measurement noise)

---

### Q15: How long should I monitor after migration?

**A**: Minimum 24 hours, but longer for production systems.

**Monitoring timeline**:
- **Hour 0-1**: Immediate validation (smoke tests)
- **Hour 1-4**: Short-term stability (integration tests)
- **Hour 4-12**: Medium-term stability (performance)
- **Hour 12-24**: Long-term stability (full monitoring)
- **Day 2-7**: Extended monitoring (optional, for critical systems)

**What to monitor**:
- Error rate (< 1%)
- Service availability (>= 99.9%)
- Memory usage (no leaks)
- Performance (within 10% of baseline)
- Customer complaints

---

### Q16: When can I delete the backup?

**A**: Wait until you're confident migration is stable.

**Timeline**:
- **After 24 hours**: Can delete temporary files
- **After 7 days**: Can delete old OTP installation
- **After 30 days**: Can delete migration backups

**Recommendation**: Keep at least 1 backup until next successful migration.

---

## Troubleshooting Questions

### Q17: What if I need to rollback?

**A**: Use the rollback procedure:

**Automated rollback**:
```bash
./scripts/rollback_otp.sh
```

**Manual rollback**:
```bash
# 1. Stop application
rebar3 release stop
killall beam.smp

# 2. Switch OTP
source ~/.erlmcp/otp-26.2.5/activate

# 3. Restore config
cp -r backups/pre_migration_*/config ./

# 4. Recompile
rebar3 clean
rebar3 compile

# 5. Start
rebar3 release start
```

**When to rollback**:
- Error rate > 5%
- Service availability < 99%
- Data corruption detected
- Critical features not working

---

### Q18: What if rollback fails?

**A**: Escalate immediately.

**Try these steps**:
1. **Rebuild from source**
   ```bash
   source ~/.erlmcp/otp-26.2.5/activate
   rebar3 clean
   rebar3 compile
   ```

2. **Restore from git**
   ```bash
   git reset --hard HEAD~1
   rebar3 compile
   ```

3. **Escalate to senior engineers**
   - Document what you tried
   - Provide logs and error messages
   - Don't make things worse

---

### Q19: Why are my tests failing after migration?

**A**: Common causes:

**Timing changes**: OTP 28 scheduler changes can affect timing
```erlang
% Fix: Use more robust assertions
?assertEqual(ok, wait_for_success(10000)),  % 10 second timeout
```

**Behavior changes**: Some APIs have subtle changes
```erlang
% Fix: Use version guards
-ifdef(OTP_MODERN).
test_new_feature() ->
    ?assert(true).
-endif.
```

**Mock failures**: Mock expectations may need updating
```erlang
% Fix: Update mock expectations
meck:expect(module, function, fun(...) -> new_result end)
```

---

### Q20: What if I see "function not exported" errors?

**A**: API may have changed between OTP versions.

**Diagnose**:
```bash
# Check what's exported
erl -noshell -eval 'Modules = code:all_loaded(), io:format("~p~n", [Modules]), halt(0).'
```

**Fix**:
1. Check OTP release notes for API changes
2. Update code to use new API
3. Add version guards for compatibility

---

## Advanced Questions

### Q21: Can I run multiple OTP versions simultaneously?

**A**: Yes, with careful setup.

**Approach 1: Separate installations**
```bash
# Install multiple OTP versions
~/.erlmcp/otp-26.2.5/
~/.erlmcp/otp-27.3.1/
~/.erlmcp/otp-28.3.1/

# Switch between them
source ~/.erlmcp/otp-26.2.5/activate  # Switch to OTP 26
source ~/.erlmcp/otp-28.3.1/activate  # Switch to OTP 28
```

**Approach 2: Version-specific releases**
```bash
# Build release with OTP 26
rebar3 as 26 release

# Build release with OTP 28
rebar3 as 28 release
```

**Use case**: Gradual migration, A/B testing, compatibility testing

---

### Q22: How do I migrate a distributed system?

**A**: Careful orchestration required.

**Strategy**:
1. **Migrate non-critical nodes first**
   - Test nodes
   - Staging nodes
   - Low-traffic nodes

2. **Validate communication** between OTP versions
   - Check protocol compatibility
   - Test distributed Erlang
   - Verify Mnesia replication

3. **Migrate critical nodes** with minimal disruption
   - Use blue-green deployment
   - Drain connections gracefully
   - Switch traffic after validation

**Key considerations**:
- OTP 26 ↔ OTP 28 communication is compatible
- Some features (priority messages) only work OTP 28 ↔ OTP 28
- Plan for mixed-version period during migration

---

### Q23: Can I use hot code loading during migration?

**A**: Not recommended for OTP version changes.

**Hot code loading** is for:
- Bug fixes
- Feature additions
- Configuration changes

**NOT suitable for**:
- OTP version changes
- Behavior changes
- Data structure changes

**Why**: OTP version affects the VM itself, not just the code.

---

### Q24: How do I handle custom NIFs (Native Implemented Functions)?

**A**: Recompile NIFs for target OTP version.

**Steps**:
1. **Rebuild NIFs**
   ```bash
   cd c_src/
   make clean
   make
   ```

2. **Verify NIF loading**
   ```erlang
   erl -noshell -eval '
       case code:load_file(my_nif) of
           {module, _} -> io:format("✅ NIF loaded~n");
           {error, Reason} -> io:format("❌ NIF failed: ~p~n", [Reason])
       end,
       halt(0).'
   ```

3. **Test NIF functionality**
   ```erlang
   my_nif:some_function()
   ```

**Note**: NIFs are often ABI-compatible within major OTP versions, but recompilation is recommended.

---

### Q25: What about LTS (Long Term Support) versions?

**A**: OTP doesn't have formal LTS releases, but:

**Stable versions**:
- OTP 26: Maintenance until 2026-06-30
- OTP 27: Maintenance until 2027-06-30
- OTP 28: Maintenance until 2028-06-30

**Recommendation**:
- Use latest stable version (OTP 28.3.1)
- Plan to upgrade annually
- Track OTP release announcements

---

## Still Have Questions?

### Resources

**Documentation**:
- [OTP 28 Release Notes](https://www.erlang.org/doc/system_principles/otp_system_guide.html)
- [Migration Plan](/Users/sac/erlmcp/docs/OTP_MIGRATION_PLAN.md)
- [Pre-Migration Checklist](/Users/sac/erlmcp/docs/migration/PRE_MIGRATION_CHECKLIST.md)

**Community**:
- [Erlang Forums](https://erlangforums.com/)
- [Erlang Slack](https://erlang.slack.com/)
- [Stack Overflow](https://stackoverflow.com/questions/tagged/erlang)

**Internal**:
- Migration Lead: [NAME]
- DevOps Lead: [NAME]
- Engineering Manager: [NAME]

### Getting Help

1. **Check this FAQ first**
2. **Search documentation**
3. **Ask in team chat**
4. **Escalate to migration lead**
5. **Create support ticket** (blocking issues)

---

**FAQ Last Updated**: 2026-02-01
**Next Review**: After migration completion

---

**END OF FAQ**
