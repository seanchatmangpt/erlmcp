# HOW TO VERIFY erlmcp

**5-MINUTE QUICK START**

```bash
cd /Users/sac/erlmcp
./scripts/verify_all.sh
```

**Expected:** 78/78 checks passed ✅

---

## WHAT WAS VERIFIED

### ✅ Structure Verification (100%)
- 121 source modules exist
- 595 test files exist
- 60+ documentation files
- 5 transport implementations
- 4 validation modules
- OTP supervision patterns

### ⚠️ Not Yet Verified
- Code compiles (2 min)
- Tests pass (10 min)
- Coverage ≥80% (5 min)
- Type checking (10 min)

---

## COMPLETE VERIFICATION (30 min)

### Step 1: Compile (2 min)
```bash
TERM=dumb rebar3 compile
```
**Expected:** 0 errors

### Step 2: Unit Tests (10 min)
```bash
rebar3 eunit --module=erlmcp_spec_parser_tests
```
**Expected:** Tests pass

### Step 3: Integration Tests (15 min)
```bash
rebar3 ct --suite=erlmcp_spec_compliance_SUITE
```
**Expected:** Tests pass

### Step 4: Coverage (5 min)
```bash
rebar3 cover
```
**Expected:** ≥80% coverage

### Step 5: Type Check (10 min)
```bash
rebar3 dialyzer
```
**Expected:** 0 warnings (ideal)

---

## INTERPRET RESULTS

### All Checks Pass ✅
**Trust Level:** VERY HIGH  
**Action:** Safe to use for development

### Some Checks Fail ❌
**Trust Level:** NEEDS ATTENTION  
**Action:** Review failures, fix critical issues

### Coverage <80% ⚠️
**Trust Level:** MODERATE  
**Action:** Add tests to reach 80% coverage

---

## FILE-BASED VERIFICATION

### Check Core Modules
```bash
ls -la apps/erlmcp_core/src/erlmcp_client.erl
ls -la apps/erlmcp_core/src/erlmcp_server.erl
ls -la apps/erlmcp_core/src/erlmcp_spec_parser.erl
```

### Check Transports
```bash
ls -la apps/erlmcp_transports/src/erlmcp_transport_*.erl
```

### Check Validators
```bash
ls -la apps/erlmcp_validation/src/erlmcp_*_validator.erl
```

### Check Functions
```bash
grep "get_version" apps/erlmcp_core/src/erlmcp_spec_parser.erl
grep "add_tool\|add_resource" apps/erlmcp_core/src/erlmcp_server.erl
grep "encode_request\|decode_message" apps/erlmcp_core/src/erlmcp_json_rpc.erl
```

---

## TROUBLESHOOTING

### Compilation Errors
```bash
# Check syntax
TERM=dumb rebar3 compile 2>&1 | grep "error"
```

### Test Failures
```bash
# Run with verbose output
rebar3 eunit --module=MODULE --verbose
```

### Coverage Issues
```bash
# Generate coverage report
rebar3 cover
# View report
open _build/test/cover/index.html
```

### Type Warnings
```bash
# Run Dialyzer
rebar3 dialyzer
# Check specific warnings
rebar3 dialyzer -Wunmatched_returns
```

---

## QUICK REFERENCE

| Task | Command | Time |
|------|---------|------|
| Structure check | `./scripts/verify_all.sh` | 1 min |
| Compile | `TERM=dumb rebar3 compile` | 2 min |
| Unit tests | `rebar3 eunit` | 10 min |
| Integration tests | `rebar3 ct` | 15 min |
| Coverage | `rebar3 cover` | 5 min |
| Type check | `rebar3 dialyzer` | 10 min |

**Total Full Verification:** ~43 minutes

---

## JOE ARMSTRONG'S ADVICE

> *"Don't trust me. Run the code. Verify it works. If it doesn't work, fix it."*

**Apply This Philosophy:**
1. ✅ Files exist → Verified
2. ⚠️ Code compiles → Run `TERM=dumb rebar3 compile`
3. ⚠️ Tests pass → Run `rebar3 eunit && rebar3 ct`
4. ⚠️ Coverage ≥80% → Run `rebar3 cover`

**Trust is earned through verification, not assertions.**

---

## DOCUMENTATION

- **FINAL_TRUST_REPORT.md** - Complete trust analysis with 250+ verification commands
- **VERIFICATION_SUMMARY.md** - Quick summary of verification results
- **HOW_TO_VERIFY.md** - This file (quick reference)

---

**Last Updated:** 2026-01-30 23:59:00  
**Verification Status:** ✅ 78/78 structure checks passed  
**Next Step:** Run compilation and tests for full verification
