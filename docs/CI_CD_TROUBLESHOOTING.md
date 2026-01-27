# TCPS CI/CD Troubleshooting Guide

**Complete troubleshooting reference for TCPS pipelines**

---

## Table of Contents

1. [Common Issues](#common-issues)
2. [GitHub Actions](#github-actions-issues)
3. [GitLab CI](#gitlab-ci-issues)
4. [Jenkins](#jenkins-issues)
5. [Pre-Commit Hooks](#pre-commit-hook-issues)
6. [Docker](#docker-issues)
7. [Quality Gates](#quality-gate-issues)
8. [Notifications](#notification-issues)
9. [Performance](#performance-issues)
10. [Emergency Procedures](#emergency-procedures)

---

## Common Issues

### Issue: SHACL Validation Fails

**Symptoms:**
```
❌ ANDON: SHACL validation failed
```

**Causes:**
1. Invalid RDF/Turtle syntax in ontology files
2. Missing required properties in TCPS instances
3. Constraint violations (cardinality, datatype, etc.)

**Solutions:**

1. **Check syntax errors:**
   ```bash
   python3 -c "from rdflib import Graph; g = Graph(); g.parse('ontology/tcps_core.ttl', format='turtle')"
   ```

2. **Run detailed SHACL validation:**
   ```bash
   cd tests/shacl
   python3 -m pytest test_tcps_validation.py -v -s
   ```

3. **Review violation reports:**
   - Look for `sh:resultMessage` in output
   - Fix missing properties or invalid values

4. **Common fixes:**
   ```turtle
   # Missing required property
   <urn:tcps:work-order:123> a tcps:WorkOrder ;
       tcps:orderID "WO-123" ;           # ✅ Required
       tcps:priority "normal" ;          # ✅ Required
       tcps:createdAt "..."^^xsd:dateTime .  # ✅ Required
   ```

---

### Issue: Compilation Errors

**Symptoms:**
```
❌ ANDON: Compilation errors detected
```

**Causes:**
1. Syntax errors in Erlang code
2. Missing dependencies
3. Type specification errors

**Solutions:**

1. **Check syntax locally:**
   ```bash
   rebar3 compile
   ```

2. **Check dependencies:**
   ```bash
   rebar3 get-deps
   rebar3 tree
   ```

3. **Check for typos:**
   ```bash
   grep -r "undefined function" _build/default/lib/*/ebin/
   ```

4. **Clean and rebuild:**
   ```bash
   rebar3 clean
   rm -rf _build
   rebar3 compile
   ```

---

### Issue: Tests Failing

**Symptoms:**
```
❌ ANDON: Tests failing
```

**Causes:**
1. Code changes broke existing tests
2. Test dependencies missing
3. Flaky tests (timing, concurrency)

**Solutions:**

1. **Run tests locally:**
   ```bash
   rebar3 as test eunit --verbose
   ```

2. **Check specific test:**
   ```bash
   rebar3 as test eunit --module=erlmcp_server_tests
   ```

3. **Enable detailed output:**
   ```bash
   rebar3 as test eunit --verbose --cover
   ```

4. **Fix flaky tests:**
   - Add timeouts: `timer:sleep(100)`
   - Use proper mocking: `meck:new(Module, [...])`
   - Isolate test state

---

### Issue: Quality Gate Failure (Coverage)

**Symptoms:**
```
❌ Quality gate FAILED: Coverage 75% < 80%
```

**Causes:**
1. New code not covered by tests
2. Tests not measuring coverage correctly
3. Coverage threshold too high for current codebase

**Solutions:**

1. **Check current coverage:**
   ```bash
   rebar3 cover --verbose
   ```

2. **Identify uncovered code:**
   ```bash
   rebar3 cover
   # Look in _build/test/cover/*.html
   ```

3. **Add missing tests:**
   ```erlang
   % Add tests for uncovered functions
   test_new_feature() ->
       Result = my_module:new_function(Input),
       ?assertEqual(Expected, Result).
   ```

4. **Temporary: Lower threshold** (not recommended):
   ```yaml
   # .github/workflows/tcps.yml
   env:
     TCPS_QUALITY_GATE_COVERAGE: 75  # Temporarily lower
   ```

   **Note:** Document plan to increase back to 80% in Kaizen report.

---

### Issue: Non-Deterministic Build

**Symptoms:**
```
❌ Build is NOT deterministic
Differences:
< abc123... file.beam
> def456... file.beam
```

**Causes:**
1. Timestamps embedded in BEAM files
2. Random data in compiled code
3. System-dependent paths
4. Erlang compiler version mismatch

**Solutions:**

1. **Check Erlang version:**
   ```bash
   erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'
   ```

   Ensure same version in CI and locally.

2. **Check for embedded timestamps:**
   ```erlang
   % BAD: Embeds current time
   -define(BUILD_TIME, calendar:local_time()).

   % GOOD: Use build-time env variable
   -define(BUILD_TIME, os:getenv("SOURCE_DATE_EPOCH")).
   ```

3. **Check rebar.config for deterministic options:**
   ```erlang
   {erl_opts, [
       deterministic,  % ✅ Add this
       debug_info
   ]}.
   ```

4. **Inspect differing files:**
   ```bash
   diff <(xxd _build/prod/lib/*/ebin/file.beam) \
        <(xxd build2/lib/*/ebin/file.beam)
   ```

---

## GitHub Actions Issues

### Issue: Workflow Not Triggering

**Symptoms:**
- Push to branch but no workflow run

**Solutions:**

1. **Check branch filters:**
   ```yaml
   on:
     push:
       branches: [ main, develop, 'feature/**' ]  # ✅ Match your branch
   ```

2. **Check file location:**
   - Must be: `.github/workflows/tcps.yml`
   - Not: `github/workflows/tcps.yml` (missing dot)

3. **Check syntax:**
   ```bash
   yamllint .github/workflows/tcps.yml
   ```

4. **Force trigger:**
   - Go to **Actions** → **TCPS Production Pipeline** → **Run workflow**

---

### Issue: Artifact Upload Fails

**Symptoms:**
```
Error: Unable to upload artifact
```

**Solutions:**

1. **Check file exists:**
   ```yaml
   - name: Upload receipt
     uses: actions/upload-artifact@v4
     with:
       name: tcps-receipt
       path: receipt.json  # ✅ Ensure file was created
   ```

2. **Use `if: always()`:**
   ```yaml
   - name: Upload receipt
     if: always()  # ✅ Upload even if step fails
     uses: actions/upload-artifact@v4
   ```

3. **Check artifact size:**
   - GitHub limit: 10 GB total per workflow
   - Compress large files: `tar -czf receipts.tar.gz receipts/`

---

## GitLab CI Issues

### Issue: Pipeline Not Starting

**Symptoms:**
- Push to GitLab but pipeline doesn't run

**Solutions:**

1. **Check .gitlab-ci.yml location:**
   - Must be in repository root
   - Exactly named: `.gitlab-ci.yml`

2. **Validate syntax:**
   ```bash
   # In GitLab UI
   CI/CD → Pipelines → CI Lint
   ```

3. **Check GitLab Runner:**
   ```bash
   # On runner host
   gitlab-runner verify
   gitlab-runner list
   ```

4. **Enable pipelines:**
   - Settings → CI/CD → General pipelines → Enable CI/CD

---

### Issue: Docker Executor Fails

**Symptoms:**
```
ERROR: Cannot connect to the Docker daemon
```

**Solutions:**

1. **Check Docker on runner:**
   ```bash
   # On runner host
   docker ps
   systemctl status docker
   ```

2. **Check runner config:**
   ```toml
   # /etc/gitlab-runner/config.toml
   [[runners]]
     executor = "docker"
     [runners.docker]
       privileged = false
       volumes = ["/cache"]
   ```

3. **Restart runner:**
   ```bash
   gitlab-runner restart
   ```

---

## Jenkins Issues

### Issue: Jenkinsfile Not Found

**Symptoms:**
```
ERROR: Jenkinsfile not found in repository
```

**Solutions:**

1. **Check file location:**
   - Must be in repository root
   - Exactly named: `Jenkinsfile` (capital J, no extension)

2. **Check pipeline configuration:**
   - Pipeline → Configure → Pipeline
   - Script Path: `Jenkinsfile`

3. **Check SCM branch:**
   - Ensure correct branch is configured

---

### Issue: Agent/Node Offline

**Symptoms:**
```
ERROR: No available executors
```

**Solutions:**

1. **Check agent status:**
   - Manage Jenkins → Manage Nodes → Select node

2. **Reconnect agent:**
   ```bash
   # On agent
   java -jar agent.jar -jnlpUrl <jenkins-url> -secret <secret>
   ```

3. **Use `agent any`:**
   ```groovy
   pipeline {
       agent any  // ✅ Use any available agent
   }
   ```

---

## Pre-Commit Hook Issues

### Issue: Hook Not Running

**Symptoms:**
- Commit succeeds without running checks

**Solutions:**

1. **Check symlink:**
   ```bash
   ls -la .git/hooks/pre-commit
   # Should point to: ../../ci/pre-commit
   ```

2. **Recreate symlink:**
   ```bash
   rm .git/hooks/pre-commit
   ln -s ../../ci/pre-commit .git/hooks/pre-commit
   ```

3. **Check executable:**
   ```bash
   chmod +x ci/pre-commit
   ```

4. **Test manually:**
   ```bash
   .git/hooks/pre-commit
   ```

---

### Issue: Hook Too Slow

**Symptoms:**
- Pre-commit takes > 30 seconds

**Solutions:**

1. **Skip slow checks:**
   ```bash
   # Edit ci/pre-commit
   # Comment out Dialyzer check for speed
   # echo "⚠️  Skipping Dialyzer (too slow)"
   ```

2. **Use CI for full checks:**
   - Pre-commit: Fast smoke tests
   - CI pipeline: Full validation

3. **Cache Dialyzer PLT:**
   ```bash
   # Build once, reuse
   rebar3 dialyzer
   # PLT cached in ~/.cache/rebar3/
   ```

---

## Docker Issues

### Issue: Build Fails in Docker

**Symptoms:**
```
ERROR: Docker build failed
```

**Solutions:**

1. **Check Dockerfile syntax:**
   ```bash
   docker build --no-cache -f ci/Dockerfile.tcps .
   ```

2. **Check base image:**
   ```dockerfile
   FROM erlang:27.1  # ✅ Ensure version exists on Docker Hub
   ```

3. **Check network:**
   ```bash
   docker build --network=host -f ci/Dockerfile.tcps .
   ```

4. **Clean Docker cache:**
   ```bash
   docker system prune -a
   ```

---

### Issue: Container Out of Memory

**Symptoms:**
```
ERROR: Container killed (OOM)
```

**Solutions:**

1. **Increase memory:**
   ```bash
   docker run --memory=4g -v $(pwd):/workspace tcps-pipeline
   ```

2. **Check Erlang memory:**
   ```dockerfile
   # In Dockerfile
   ENV ERL_MAX_HEAP_SIZE=2147483648  # 2GB
   ```

3. **Split pipeline stages:**
   - Run one stage at a time instead of full pipeline

---

## Quality Gate Issues

### Issue: Cannot Meet Coverage Threshold

**Symptoms:**
- Stuck at 70% coverage, need 80%

**Solutions:**

1. **Identify gaps:**
   ```bash
   rebar3 cover --verbose
   # Open HTML reports in _build/test/cover/
   ```

2. **Add tests systematically:**
   - Test one module at a time
   - Focus on business logic first
   - Use property-based testing (PropEr)

3. **Exclude generated code:**
   ```erlang
   % rebar.config
   {cover_excl_mods, [
       generated_parser,  % Exclude from coverage
       legacy_module
   ]}.
   ```

4. **Incremental improvement:**
   - Start: 70% threshold
   - After 2 weeks: 75%
   - After 1 month: 80%

   Document in Kaizen report.

---

## Notification Issues

### Issue: Slack Notifications Not Sending

**Symptoms:**
- No Slack messages on Andon events

**Solutions:**

1. **Check webhook URL:**
   ```bash
   curl -X POST -H 'Content-Type: application/json' \
     -d '{"text":"Test"}' $SLACK_WEBHOOK_URL
   ```

2. **Check environment variable:**
   ```bash
   # GitHub Actions
   Settings → Secrets → SLACK_WEBHOOK_URL

   # GitLab CI
   Settings → CI/CD → Variables → SLACK_WEBHOOK_URL

   # Jenkins
   Credentials → slack-webhook
   ```

3. **Test notification script:**
   ```bash
   export SLACK_WEBHOOK_URL="https://hooks.slack.com/..."
   ./ci/tcps-notify.sh info low "Test message" WO-TEST
   ```

4. **Check Slack app permissions:**
   - Incoming Webhooks must be enabled

---

### Issue: Email Notifications Failing

**Symptoms:**
```
❌ Failed to send email: Authentication failed
```

**Solutions:**

1. **Check SMTP credentials:**
   ```bash
   export EMAIL_USERNAME="your-email@gmail.com"
   export EMAIL_PASSWORD="your-app-password"  # Not regular password!
   ```

2. **Use Gmail App Password:**
   - Go to: https://myaccount.google.com/apppasswords
   - Generate app password (not regular Gmail password)

3. **Check SMTP server:**
   ```bash
   telnet smtp.gmail.com 587
   # Should connect
   ```

4. **Test Python SMTP:**
   ```python
   import smtplib
   server = smtplib.SMTP("smtp.gmail.com", 587)
   server.starttls()
   server.login("user@gmail.com", "app-password")
   server.quit()
   ```

---

## Performance Issues

### Issue: Pipeline Too Slow

**Symptoms:**
- Pipeline takes > 15 minutes

**Solutions:**

1. **Profile stages:**
   - Check Kaizen report for takt time breakdown
   - Identify stages taking > 10% of total time

2. **Cache dependencies:**
   ```yaml
   # GitHub Actions
   - uses: actions/cache@v4
     with:
       path: _build
       key: ${{ runner.os }}-erlang-${{ hashFiles('rebar.lock') }}
   ```

3. **Parallelize tests:**
   ```erlang
   % rebar.config
   {eunit_opts, [{report, {eunit_progress, [colored, profile]}}]}.
   ```

4. **Skip slow checks in PR:**
   ```yaml
   # Only run deterministic build on main
   if: github.ref == 'refs/heads/main'
   ```

5. **Use faster runners:**
   - GitHub Actions: Larger runners
   - GitLab CI: Dedicated runners
   - Jenkins: More executors

---

## Emergency Procedures

### Emergency: Production Outage, Need to Skip Quality Gates

**Procedure:**

1. **Document reason:**
   ```bash
   echo "EMERGENCY: Production outage - Skipping quality gates" > EMERGENCY.txt
   git add EMERGENCY.txt
   ```

2. **Skip pre-commit:**
   ```bash
   git commit --no-verify -m "Emergency fix: <description>"
   ```

3. **Skip CI Andon:**
   - GitHub Actions: Workflow dispatch with `skip_andon: true`
   - GitLab CI: Set variable `TCPS_SKIP_ANDON=true`
   - Jenkins: Pass parameter

4. **Push directly:**
   ```bash
   git push origin main
   ```

5. **Post-incident:**
   - File incident report
   - Root cause analysis (5 Whys)
   - Update standard work to prevent recurrence
   - Run full quality checks ASAP

**Template Incident Report:**

```markdown
# Incident Report: TCPS Quality Gate Skip

**Date**: 2026-01-26
**Severity**: Critical
**Duration**: 30 minutes

## Summary
Production outage required emergency deployment skipping TCPS quality gates.

## Timeline
- 10:00 - Production API down
- 10:05 - Root cause identified
- 10:10 - Fix prepared
- 10:15 - TCPS quality gates skipped, deployed
- 10:30 - Service restored

## Root Cause
Database connection pool exhausted.

## Resolution
Increased connection pool size from 10 to 50.

## Prevention
1. Add monitoring for connection pool usage
2. Add load test for high concurrency
3. Document connection pool tuning

## Quality Debt
- Skipped: SHACL validation, coverage check
- Action: Run full TCPS pipeline on next commit
- Target: Within 24 hours

**Signed:** Team Lead
```

---

## Getting Help

If troubleshooting doesn't resolve your issue:

1. **Check logs:**
   - GitHub Actions: Actions tab → Job logs
   - GitLab CI: Pipelines → Job → View logs
   - Jenkins: Build → Console Output

2. **Enable debug mode:**
   ```bash
   # GitHub Actions
   ACTIONS_STEP_DEBUG=true

   # GitLab CI
   CI_DEBUG_TRACE=true

   # Bash scripts
   bash -x ci/run-tcps-pipeline.sh
   ```

3. **File an issue:**
   - Include: Pipeline logs, error messages, configuration
   - Redact: Secrets, credentials, sensitive data

4. **Team communication:**
   - Slack: `#tcps-pipeline`
   - Email: tcps-team@example.com

---

**Last Updated:** 2026-01-26
**Version:** 1.0.0
