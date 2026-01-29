# Dictionary Attack Test - Quick Start Guide

## Overview

The dictionary attack stress test (#15) simulates rapid reconnections with random IDs to find DoS vulnerabilities in the erlmcp authentication system.

## Quick Start

### 1. Basic Test (100 attempts, 50/sec)

```bash
cd /Users/sac/erlmcp/bench
./dictionary_attack_test.erl 10015 50 100 mixed
```

**Expected Output**:
```
=== DICTIONARY ATTACK STRESS TEST #15 ===
Attack: Rapid Reconnect with Random IDs
Configuration:
  Port: 10015
  Rate: 50 connections/sec
  Total Attempts: 100
  Credential Type: mixed

=== ATTACK COMPLETE ===
RATE LIMITING:
- Limit Detected: NO  <-- Should be YES after implementing rate limiting
```

### 2. Rate Limit Discovery (10,000 attempts)

```bash
./dictionary_attack_test.erl 10015 100 10000 valid
```

**Purpose**: Find the rate limit threshold  
**Expected**: Rate limiting should trigger within first 100-1000 attempts

### 3. Session Flooding Attack

```bash
./dictionary_attack_test.erl 10015 100 1000 same_credential
```

**Purpose**: Test session limits per user  
**Expected**: Should fail after 10 concurrent sessions (if implemented)

### 4. Sustained Attack (100,000 attempts)

```bash
./dictionary_attack_test.erl 10015 100 100000 mixed
```

**Purpose**: Test for memory leaks and degradation  
**Duration**: ~16 minutes  
**Expected**: Memory usage should remain stable

### 5. Full Dictionary Attack (1,000,000 attempts)

```bash
./dictionary_attack_test.erl 10015 1000 1000000 mixed
```

**Purpose**: Complete attack simulation  
**Duration**: ~16 minutes  
**Expected**: Rate limiting should throttle attack immediately

## Test Parameters

```
Usage: ./dictionary_attack_test.erl [Port] [Rate] [Attempts] [CredentialType]

Arguments:
  Port            - TCP port (default: 10015)
  Rate            - Connections per second (default: 100)
  Attempts        - Total connection attempts (default: 10000)
  CredentialType  - valid | invalid | mixed | same_credential (default: mixed)

Examples:
  ./dictionary_attack_test.erl 10015 50 1000 mixed
  ./dictionary_attack_test.erl 10015 100 50000 valid
  ./dictionary_attack_test.erl 10015 200 100000 same_credential
```

## Credential Types

| Type | Description | Use Case |
|------|-------------|----------|
| `valid` | Use known valid API key | Test rate limiting threshold |
| `invalid` | Random 32-byte keys | Test noise flooding |
| `mixed` | 50% valid, 50% invalid | Realistic credential stuffing |
| `same_credential` | Same key, different IDs | Test session limits |

## Interpreting Results

### Good Result (System Protected)

```
RATE LIMITING:
- Limit Detected: yes
- Rate Limited Count: 9900
- LIMIT EFFECTIVENESS: Attack was throttled

ANALYSIS:
- Rate limiting is WORKING - system protected
- DoS resistance: GOOD
```

### Bad Result (System Vulnerable)

```
RATE LIMITING:
- Limit Detected: NO
- WARNING: System vulnerable to brute force

ANALYSIS:
- Rate limiting NOT DETECTED - system VULNERABLE
- DoS resistance: POOR - implement rate limiting immediately
```

## Common Issues

### Issue: "connection errors"

**Cause**: No MCP server running on target port  
**Solution**: Start an MCP server first, or use with a test server

### Issue: "Rate limit not detected"

**Cause**: Rate limiting not implemented  
**Solution**: Implement rate limiting (see REMEDIATION section)

### Issue: "Memory leak detected"

**Cause**: Sessions not being cleaned up  
**Solution**: Check session cleanup logic in `erlmcp_auth.erl`

## Validation Checklist

Before deploying to production:

- [ ] Rate limiting detected within first 100 attempts
- [ ] Authentication success rate < 1% for invalid credentials
- [ ] Memory usage stable (< 5% increase)
- [ ] No server crashes during sustained attack
- [ ] Clear logs showing rate limiting in action
- [ ] Session limits enforced (max 10 per user)
- [ ] IP blocking works after repeated failures

## Files Generated

Test results are saved to:
```
bench/results/dictionary_attack_{CREDENTIAL_TYPE}_{TIMESTAMP}.txt
```

Example:
```
bench/results/dictionary_attack_mixed_1769714810.txt
```

## Next Steps

1. **Run Tests**: Execute all test scenarios
2. **Review Results**: Check for rate limiting and memory leaks
3. **Implement Fixes**: Add rate limiting if not present
4. **Re-test**: Validate fixes with same tests
5. **Monitor**: Set up continuous monitoring for auth attacks

## Support

For issues or questions:
- See: `DICTIONARY_ATTACK_FINAL_REPORT.md` for detailed analysis
- See: `DICTIONARY_ATTACK_RESULTS.md` for vulnerability details
- Code: `erlmcp_bench_dictionary_attack.erl`

---

**Last Updated**: 2025-01-29  
**Test Version**: 1.0.0  
**Severity**: CRITICAL - Immediate Action Required
