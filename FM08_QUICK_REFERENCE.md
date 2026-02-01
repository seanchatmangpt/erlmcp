# FM-08 Secret Redaction - Quick Reference

## Files Created

### Production Code
1. `apps/erlmcp_core/src/erlmcp_logging_redactor.erl` (10K)
   - Main redaction module with pattern matching and recursive redaction

2. `apps/erlmcp_core/src/erlmcp_logging.erl` (14K - modified)
   - Integrated redaction at lines 290-292 in do_log/6

### Test Code
3. `apps/erlmcp_core/test/erlmcp_logging_redaction_tests.erl` (17K)
   - 16 unit tests + 2 integration tests

4. `apps/erlmcp_core/test/test_secret_scan_SUITE.erl` (13K)
   - 7 CI quality gate tests

### Configuration
5. `config/sys.config` (modified)
   - Added logging_redaction section after line 4

### Documentation
6. `FM08_SECRET_REDACTION_IMPLEMENTATION.md` (15K)
   - Complete implementation guide and documentation

## Testing Commands

```bash
# Compile
rebar3 compile

# Run inline tests in redactor module
rebar3 eunit --module=erlmcp_logging_redactor

# Run comprehensive unit tests
rebar3 eunit --module=erlmcp_logging_redaction_tests

# Run CI secret scan (quality gate)
rebar3 ct --suite=test_secret_scan_SUITE

# Type checking
rebar3 dialyzer

# Cross-reference validation
rebar3 xref

# All quality gates
make check
```

## Configuration Quick Ref

```erlang
% Enable/disable (in sys.config)
{logging_redaction, #{enabled => true}}

% At runtime
application:set_env(erlmcp, logging_redaction, #{enabled => false}).

% Check if redaction is working
1> erlmcp_logging_redactor:redact_message(<<"password=secret123">>).
<<"***REDACTED***">>
```

## Secret Types Covered

1. JWT Tokens: `Bearer eyJhbGci...`, `xxx.yyy.zzz`
2. Session IDs: `550e8400-e29b-41d4-a716-446655440000`
3. API Keys: `sk_live_...`, `pk_test_...`, 32+ char keys
4. AWS: `AKIAIOSFODNN7EXAMPLE`, 40-char secrets
5. OAuth: `ya29.a0AfH6SMBx...`
6. Passwords: `password`, `secret` map keys
7. Custom: Configurable regex patterns

## Key Functions

```erlang
% Redact any term
erlmcp_logging_redactor:redact_message(Term).

% Redact map data
erlmcp_logging_redactor:redact_data(Map).

% Check if key is secret
erlmcp_logging_redactor:is_secret_key(<<"password">>).  % true
erlmcp_logging_redactor:is_secret_key(<<"username">>).  % false

% Get default patterns
erlmcp_logging_redactor:default_patterns().
```

## CI Integration

### GitHub Actions

```yaml
name: Security - Secret Scan
on: [push, pull_request]
jobs:
  secret-scan:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Setup Erlang
        uses: erlef/setup-beam@v1
        with:
          otp-version: '25.0'
      - name: Compile
        run: rebar3 compile
      - name: Run Secret Scan
        run: rebar3 ct --suite=test_secret_scan_SUITE
      # Build BLOCKED if secrets detected
```

## Monitoring

### Metrics

```erlang
% Log redaction statistics
{ok, Stats} = erlmcp_logging:get_stats().
% Check for redaction_count metric

% Performance impact
% Expected: < 50 microseconds per log entry
```

### SIEM Alerts

**Splunk**:
```spl
index=erlmcp source="erlmcp.log"
| regex _raw="(Bearer\s+[A-Za-z0-9_\-\.]+|AKIA[0-9A-Z]{16})"
| stats count
| where count > 0
```

Alert if count > 0 (critical - secrets leaked)

## Rollback

If redaction causes issues:

```erlang
% Emergency disable (requires restart)
% In sys.config:
{logging_redaction, #{enabled => false}}

% Hot reload (without restart)
application:set_env(erlmcp, logging_redaction, #{enabled => false}).

% Re-enable after fix
application:set_env(erlmcp, logging_redaction, #{enabled => true}).
```

## Troubleshooting

### Secret still appearing in logs?

1. Check redaction enabled:
   ```erlang
   application:get_env(erlmcp, logging_redaction).
   ```

2. Check pattern matches:
   ```erlang
   Pattern = <<"Bearer\\s+[A-Za-z0-9_\\-\\.]+">>,
   re:run(YourSecret, Pattern).
   ```

3. Add custom pattern:
   ```erlang
   Config = #{
       enabled => true,
       patterns => [{custom, <<"your-pattern">>} | DefaultPatterns],
       replacement => <<"***REDACTED***">>,
       secret_keys => DefaultKeys
   },
   application:set_env(erlmcp, logging_redaction, Config).
   ```

### Performance issues?

1. Check redaction overhead:
   ```erlang
   timer:tc(erlmcp_logging_redactor, redact_message, [YourMessage]).
   % Should be < 50 microseconds
   ```

2. Reduce pattern count or optimize regexes

3. Disable in dev (NOT in production):
   ```erlang
   {logging_redaction, #{enabled => false}}
   ```

## Success Criteria Checklist

- [ ] Compilation: 0 errors
- [ ] Unit tests: 18/18 PASS
- [ ] CI scan: 7/7 PASS
- [ ] Dialyzer: 0 warnings
- [ ] Xref: 0 undefined
- [ ] Manual test: Secrets redacted in logs
- [ ] Production config: enabled => true
- [ ] SIEM alert: No secrets detected

## Support

**Documentation**: See `FM08_SECRET_REDACTION_IMPLEMENTATION.md`
**Tests**: `apps/erlmcp_core/test/erlmcp_logging_redaction_tests.erl`
**CI Gates**: `apps/erlmcp_core/test/test_secret_scan_SUITE.erl`
**Module**: `apps/erlmcp_core/src/erlmcp_logging_redactor.erl`
