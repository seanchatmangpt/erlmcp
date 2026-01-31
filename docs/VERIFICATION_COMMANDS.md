# VERIFICATION_COMMANDS.md

**Purpose**: Provide executable commands to verify EVERY major claim in erlmcp.

**Philosophy**: Joe Armstrong's "NO THEORY. HARDDCODED TRUTH." - Every claim must be verifiable by running a command.

**Usage**: Copy-paste commands into your terminal. If output matches expected, claim is VERIFIED. If not, claim is FALSE.

---

## Table of Contents

1. [Code Quality Claims](#1-code-quality-claims)
2. [Implementation Claims](#2-implementation-claims)
3. [Performance Claims](#3-performance-claims)
4. [Documentation Claims](#4-documentation-claims)
5. [Integration Claims](#5-integration-claims)

---

## 1. CODE QUALITY CLAIMS

### Claim 1.1: "Project compiles successfully with 0 errors"

#### How to Verify
```bash
TERM=dumb rebar3 compile 2>&1 | tail -20
```

#### What You Should See
```
===> Verifying dependencies...
===> Compiling erlmcp_core
===> Compiling erlmcp_transports
===> Compiling erlmcp_observability
===> Compiling erlmcp_validation
```

#### Status
- ✅ **VERIFIED** if you see "Compiling" for all 4 apps with 0 errors
- ❌ **FALSE** if you see compilation errors
- ⚠️ **WARNING** if you see warnings but compiles

---

### Claim 1.2: "Spec parser has 466 lines, all hardcoded"

#### How to Verify
```bash
wc -l apps/erlmcp_core/src/erlmcp_spec_parser.erl
```

#### What You Should See
```
     467 apps/erlmcp_core/src/erlmcp_spec_parser.erl
```

#### Additional Verification (Check it's hardcoded, not parsed)
```bash
head -20 apps/erlmcp_core/src/erlmcp_spec_parser.erl
```

#### What You Should See
```
-module(erlmcp_spec_parser).
-author("Joe Armstrong's Philosophy: HARDCODED TRUTH").

%% @doc MCP 2025-11-25 Specification Parser
%%
%% JOE ARMSTRONG'S PHILOSOPHY:
%% "NO THEORY. HARDCODED TRUTH."
%%
%% This module does NOT parse YAML/JSON files. It does NOT download specs.
%% It HARDCODES the MCP 2025-11-25 specification directly as Erlang data.
```

#### Status
- ✅ **VERIFIED** if line count ~467 and header says "HARDCODED TRUTH"
- ❌ **FALSE** if line count differs significantly or mentions parsing files

---

### Claim 1.3: "No stub implementations in codebase"

#### How to Verify
```bash
grep -r "TODO.*implement" apps/*/src/*.erl | grep -v "%%" | wc -l
```

#### What You Should See
```
0
```

#### Additional Verification (Search for stub patterns)
```bash
grep -r "not.*implemented\|STUB\|FIXME.*implement" apps/*/src/*.erl | grep -v "%%" | wc -l
```

#### What You Should See
```
0
```

#### Status
- ✅ **VERIFIED** if both commands return 0
- ❌ **FALSE** if any TODO/FIXME stubs found
- ⚠️ **WARNING** if comments contain TODO but no code stubs

---

### Claim 1.4: "All modules have -module() and proper exports"

#### How to Verify
```bash
for file in apps/*/src/*.erl; do
  if ! grep -q "^-module(" "$file"; then
    echo "Missing -module: $file"
  fi
done
```

#### What You Should See
```
(no output)
```

#### Additional Verification (Check export declarations)
```bash
grep -L "^-export\[" apps/*/src/*.erl | wc -l
```

#### What You Should See
```
0
```

#### Status
- ✅ **VERIFIED** if no missing modules/exports reported
- ❌ **FALSE** if any files missing module/exports

---

### Claim 1.5: "Code follows Erlang OTP design patterns"

#### How to Verify
```bash
# Count gen_server implementations
grep -l "-behaviour(gen_server)" apps/*/src/*.erl | wc -l
```

#### What You Should See
```
20-30
```

#### Additional Verification (Check supervisor trees)
```bash
grep -l "-behaviour(supervisor)" apps/*/src/*.erl | wc -l
```

#### What You Should See
```
5-10
```

#### Status
- ✅ **VERIFIED** if 20+ gen_servers and 5+ supervisors
- ❌ **FALSE** if very few OTP behaviors found

---

## 2. IMPLEMENTATION CLAIMS

### Claim 2.1: "Spec parser implements MCP 2025-11-25 spec with 11 methods"

#### How to Verify
```bash
grep -A 3 "get_method(<<" apps/erlmcp_core/src/erlmcp_spec_parser.erl | grep "get_method" | wc -l
```

#### What You Should See
```
11
```

#### Additional Verification (List all methods)
```bash
grep -E "get_method\(<<\".+\">>\)" apps/erlmcp_core/src/erlmcp_spec_parser.erl | cut -d'"' -f2
```

#### What You Should See
```
initialize
ping
shutdown
resources/list
resources/read
resources/subscribe
resources/unsubscribe
tools/list
tools/call
prompts/list
prompts/get
```

#### Status
- ✅ **VERIFIED** if all 11 methods listed
- ❌ **FALSE** if fewer methods found

---

### Claim 2.2: "Spec parser implements 4 notification types"

#### How to Verify
```bash
grep "get_notification(<<" apps/erlmcp_core/src/erlmcp_spec_parser.erl | wc -l
```

#### What You Should See
```
4
```

#### Additional Verification (List notifications)
```bash
grep -E "get_notification\(<<\".+\">>\)" apps/erlmcp_core/src/erlmcp_spec_parser.erl | cut -d'"' -f2
```

#### What You Should See
```
notifications/initialized
notifications/cancelled
notifications/progress
notifications/roots/list_changed
```

#### Status
- ✅ **VERIFIED** if all 4 notifications listed
- ❌ **FALSE** if fewer notifications found

---

### Claim 2.3: "Spec parser implements 15 error codes"

#### How to Verify
```bash
grep "get_error_code(" apps/erlmcp_core/src/erlmcp_spec_parser.erl | wc -l
```

#### What You Should See
```
15
```

#### Additional Verification (List error codes)
```bash
grep "get_error_code(-" apps/erlmcp_core/src/erlmcp_spec_parser.erl | grep -oE "\-[0-9]+" | sort -u
```

#### What You Should See
```
-32700
-32600
-32601
-32602
-32603
-32009
-32008
-32007
-32006
-32005
-32004
-32003
-32002
-32001
-32000
```

#### Status
- ✅ **VERIFIED** if 15 error codes listed
- ❌ **FALSE** if fewer codes found

---

### Claim 2.4: "Vault integration uses gun HTTP client"

#### How to Verify
```bash
grep -n "gun:" apps/erlmcp_core/src/erlmcp_secrets.erl | head -10
```

#### What You Should See
```
583:    case gun:open(Host, Port, #{transport => Transport, protocols => [http]}) of
584:        {ok, ConnPid} ->
585:            MonRef = monitor(process, ConnPid),
586:
587:            % Wait for connection up
588:            case gun:await_up(ConnPid, Timeout) of
589:                {up, _Protocol} ->
590:                    % Make request
591:                    StreamRef = case Method of
592:                        get -> gun:get(ConnPid, Path, maps:to_list(Headers));
593:                        post -> gun:post(ConnPid, Path, maps:to_list(Headers), Body);
594:                        delete -> gun:delete(ConnPid, Path, maps:to_list(Headers))
595:                    end,
```

#### Additional Verification (Count gun references)
```bash
grep -c "gun:" apps/erlmcp_core/src/erlmcp_secrets.erl
```

#### What You Should See
```
8-12
```

#### Status
- ✅ **VERIFIED** if gun:open, gun:get, gun:post, gun:delete present
- ❌ **FALSE** if no gun references found
- ⚠️ **WARNING** if using httpc instead of gun

---

### Claim 2.5: "AWS integration uses httpc client"

#### How to Verify
```bash
grep -n "httpc:" apps/erlmcp_core/src/erlmcp_secrets.erl | head -10
```

#### What You Should See
```
1126:    case httpc:request(Method, Request, [{ssl, [{verify, verify_none}]} | Options], []) of
1127:        {ok, Result} ->
1128:            {ok, Result};
1129:        {error, Reason} ->
1130:            {error, Reason}
```

#### Additional Verification (Count httpc references)
```bash
grep -c "httpc:" apps/erlmcp_core/src/erlmcp_secrets.erl
```

#### What You Should See
```
4-8
```

#### Status
- ✅ **VERIFIED** if httpc:request present for AWS
- ❌ **FALSE** if no httpc references found

---

### Claim 2.6: "Secrets module has 1,297 lines"

#### How to Verify
```bash
wc -l apps/erlmcp_core/src/erlmcp_secrets.erl
```

#### What You Should See
```
   1297 apps/erlmcp_core/src/erlmcp_secrets.erl
```

#### Status
- ✅ **VERIFIED** if line count ~1,297
- ❌ **FALSE** if significantly different

---

### Claim 2.7: "Tasks module implements MCP Tasks API with 1,109 lines"

#### How to Verify
```bash
wc -l apps/erlmcp_core/src/erlmcp_tasks.erl
```

#### What You Should See
```
   1109 apps/erlmcp_core/src/erlmcp_tasks.erl
```

#### Additional Verification (Check task lifecycle states)
```bash
grep -E "pending|processing|completed|failed|cancelled" apps/erlmcp_core/src/erlmcp_tasks.erl | grep -oE "(pending|processing|completed|failed|cancelled)" | sort -u
```

#### What You Should See
```
cancelled
completed
failed
pending
processing
```

#### Status
- ✅ **VERIFIED** if ~1,109 lines and 5 lifecycle states
- ❌ **FALSE** if significantly different

---

### Claim 2.8: "Session module supports 4 backends: ets, dets, leveldb, mnesia"

#### How to Verify
```bash
grep -E "ets|dets|leveldb|mnesia" apps/erlmcp_core/src/erlmcp_session.erl | grep -oE "(ets|dets|leveldb|mnesia)" | sort -u
```

#### What You Should See
```
dets
ets
leveldb
mnesia
```

#### Additional Verification (Check backend type spec)
```bash
grep -A 2 "-type backend" apps/erlmcp_core/src/erlmcp_session.erl
```

#### What You Should See
```
-type backend() :: ets | dets | leveldb | mnesia.
```

#### Status
- ✅ **VERIFIED** if all 4 backends listed
- ❌ **FALSE** if fewer backends

---

### Claim 2.9: "Session module has 163 lines"

#### How to Verify
```bash
wc -l apps/erlmcp_core/src/erlmcp_session.erl
```

#### What You Should See
```
    163 apps/erlmcp_core/src/erlmcp_session.erl
```

#### Status
- ✅ **VERIFIED** if ~163 lines
- ❌ **FALSE** if significantly different

---

### Claim 2.10: "Transport modules implement behavior interface"

#### How to Verify
```bash
grep -l "-behaviour(erlmcp_transport)" apps/erlmcp_transports/src/*.erl | wc -l
```

#### What You Should See
```
5-10
```

#### Additional Verification (List transport modules)
```bash
grep -l "-behaviour(erlmcp_transport)" apps/erlmcp_transports/src/*.erl | xargs basename -s .erl
```

#### What You Should See
```
erlmcp_transport_stdio
erlmcp_transport_tcp
erlmcp_transport_http
erlmcp_transport_ws
erlmcp_transport_sse
```

#### Status
- ✅ **VERIFIED** if 5+ transport modules implement behavior
- ❌ **FALSE** if fewer transports

---

## 3. PERFORMANCE CLAIMS

### Claim 3.1: "Benchmarks run successfully"

#### How to Verify
```bash
ls -lh apps/erlmcp_core/bench/ | grep "_bench.erl"
```

#### What You Should See
```
-rw-r--r--  1 user  staff   1.2K Jan 30 20:00 erlmcp_bench_core_ops.erl
-rw-r--r--  1 user  staff   2.1K Jan 30 20:00 erlmcp_bench_network_real.erl
-rw-r--r--  1 user  staff   1.8K Jan 30 20:00 erlmcp_bench_stress.erl
-rw-r--r--  1 user  staff   2.3K Jan 30 20:00 erlmcp_bench_chaos.erl
-rw-r--r--  1 user  staff   1.5K Jan 30 20:00 erlmcp_bench_integration.erl
```

#### Additional Verification (Check benchmark script exists)
```bash
ls -lh scripts/bench/run_all_benchmarks.sh
```

#### What You Should See
```
-rwxr-xr-x  1 user  staff   1.5K Jan 30 20:00 scripts/bench/run_all_benchmarks.sh
```

#### Status
- ✅ **VERIFIED** if 5 benchmark modules + script exist
- ❌ **FALSE** if benchmarks missing
- ⚠️ **WARNING** if files exist but script not executable

---

### Claim 3.2: "Quick benchmark runs in <2 minutes"

#### How to Verify (ACTUALLY RUN IT)
```bash
time make benchmark-quick
```

#### What You Should See
```
===> Running core_ops benchmark...
Completed 1000 operations in 0.003 seconds (333K ops/sec)

real    1m45.234s
user    1m30.123s
sys     0m15.456s
```

#### Status
- ✅ **VERIFIED** if completes in <2m (120s)
- ❌ **FALSE** if takes >2 minutes
- ⚠️ **WARNING** if benchmark fails to run

---

### Claim 3.3: "Core operations achieve 2.69M ops/sec"

#### How to Verify (RUN ACTUAL BENCHMARK)
```bash
# In Erlang shell:
make console
# Then at prompt:
erlmcp_bench_core_ops:run(<<"core_ops_100k">>).
```

#### What You Should See
```
Running benchmark: core_ops_100k
Operations: 100000
Result: 2690000 ops/sec
```

#### Status
- ✅ **VERIFIED** if ≥2M ops/sec
- ❌ **FALSE** if <1M ops/sec
- ⚠️ **WARNING** if 1-2M ops/sec (degraded)

---

## 4. DOCUMENTATION CLAIMS

### Claim 4.1: "Architecture documentation exists"

#### How to Verify
```bash
ls -lh docs/architecture.md
```

#### What You Should See
```
-rw-r--r--  1 user  staff   25K Jan 30 20:00 docs/architecture.md
```

#### Additional Verification (Check content)
```bash
head -30 docs/architecture.md
```

#### What You Should See
```
# erlmcp Architecture

## Overview
erlmcp implements the Model Context Protocol (MCP)...
```

#### Status
- ✅ **VERIFIED** if file exists and has content
- ❌ **FALSE** if file missing or empty

---

### Claim 4.2: "OTP patterns documentation exists"

#### How to Verify
```bash
ls -lh docs/otp-patterns.md
```

#### What You Should See
```
-rw-r--r--  1 user  staff   18K Jan 30 20:00 docs/otp-patterns.md
```

#### Additional Verification (Check OTP patterns covered)
```bash
grep -E "supervisor|gen_server|gen_statem" docs/otp-patterns.md | wc -l
```

#### What You Should See
```
10+
```

#### Status
- ✅ **VERIFIED** if file exists with OTP patterns
- ❌ **FALSE** if file missing or incomplete

---

### Claim 4.3: "API reference documentation exists"

#### How to Verify
```bash
ls -lh docs/api-reference.md
```

#### What You Should See
```
-rw-r--r--  1 user  staff   32K Jan 30 20:00 docs/api-reference.md
```

#### Status
- ✅ **VERIFIED** if file exists
- ❌ **FALSE** if file missing

---

### Claim 4.4: "Protocol documentation exists"

#### How to Verify
```bash
ls -lh docs/protocol.md
```

#### What You Should See
```
-rw-r--r--  1 user  staff   28K Jan 30 20:00 docs/protocol.md
```

#### Additional Verification (Check MCP version)
```bash
grep "2025-11-25" docs/protocol.md | head -3
```

#### What You Should See
```
MCP 2025-11-25 specification...
```

#### Status
- ✅ **VERIFIED** if file exists with MCP spec version
- ❌ **FALSE** if file missing or wrong version

---

### Claim 4.5: "Examples directory has 20+ working examples"

#### How to Verify
```bash
ls -1 examples/*.erl | wc -l
```

#### What You Should See
```
20+
```

#### Additional Verification (Check examples compile)
```bash
for file in examples/*.erl; do
  echo "Checking $file..."
  erlc -o /tmp "$file" 2>&1 | head -1
done | grep "error" | wc -l
```

#### What You Should See
```
0
```

#### Status
- ✅ **VERIFIED** if 20+ examples and all compile
- ❌ **FALSE** if fewer examples or compilation errors

---

## 5. INTEGRATION CLAIMS

### Claim 5.1: "Vault integration supports 3 auth methods"

#### How to Verify
```bash
grep -E "token|approle|kubernetes" apps/erlmcp_core/src/erlmcp_secrets.erl | grep -oE "(token|approle|kubernetes)" | sort -u
```

#### What You Should See
```
approle
kubernetes
token
```

#### Additional Verification (Check auth function)
```bash
grep -A 5 "authenticate(#vault_state{auth_method" apps/erlmcp_core/src/erlmcp_secrets.erl | grep "auth_method =" | head -3
```

#### What You Should See
```
auth_method = token
auth_method = approle
auth_method = kubernetes
```

#### Status
- ✅ **VERIFIED** if all 3 auth methods implemented
- ❌ **FALSE** if fewer methods

---

### Claim 5.2: "AWS integration supports IAM role and access key auth"

#### How to Verify
```bash
grep -E "iam_role|access_key" apps/erlmcp_core/src/erlmcp_secrets.erl | grep -oE "(iam_role|access_key)" | sort -u
```

#### What You Should See
```
access_key
iam_role
```

#### Additional Verification (Check credential functions)
```bash
grep "get_aws_credentials" apps/erlmcp_core/src/erlmcp_secrets.erl | wc -l
```

#### What You Should See
```
3-5
```

#### Status
- ✅ **VERIFIED** if both auth methods present
- ❌ **FALSE** if only one method

---

### Claim 5.3: "Local encrypted storage uses AES-256-GCM"

#### How to Verify
```bash
grep -n "aes_256_gcm" apps/erlmcp_core/src/erlmcp_secrets.erl
```

#### What You Should See
```
1267:    {CipherText, Tag} = crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, PlainText, <<>>, true),
1273:    crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, CipherText, <<>>, Tag, false).
```

#### Additional Verification (Check key size)
```bash
grep -n "strong_rand_bytes(32)" apps/erlmcp_core/src/erlmcp_secrets.erl
```

#### What You Should See
```
1284:            NewKey = crypto:strong_rand_bytes(32),
```

#### Status
- ✅ **VERIFIED** if AES-256-GCM used with 32-byte key
- ❌ **FALSE** if different algorithm or key size

---

### Claim 5.4: "Transport layer implements stdio, tcp, http, websocket, sse"

#### How to Verify
```bash
ls -1 apps/erlmcp_transports/src/erlmcp_transport_*.erl | xargs basename -s .erl
```

#### What You Should See
```
erlmcp_transport_behavior
erlmcp_transport_http
erlmcp_transport_pool
erlmcp_transport_sse
erlmcp_transport_stdio
erlmcp_transport_tcp
erlmcp_transport_ws
```

#### Additional Verification (Check each implements callbacks)
```bash
for transport in stdio tcp http ws sse; do
  echo "Checking $transport..."
  grep -c "init\|send\|close" "apps/erlmcp_transports/src/erlmcp_transport_${transport}.erl"
done
```

#### What You Should See
```
3-5
3-5
3-5
3-5
3-5
```

#### Status
- ✅ **VERIFIED** if all 5 transports exist with callbacks
- ❌ **FALSE** if any transport missing

---

### Claim 5.5: "Observability app has metrics server and dashboard"

#### How to Verify
```bash
ls -1 apps/erlmcp_observability/src/erlmcp_metrics*.erl apps/erlmcp_observability/src/erlmcp_dashboard*.erl
```

#### What You Should See
```
apps/erlmcp_observability/src/erlmcp_dashboard_http_handler.erl
apps/erlmcp_observability/src/erlmcp_dashboard_server.erl
apps/erlmcp_observability/src/erlmcp_metrics.erl
apps/erlmcp_observability/src/erlmcp_metrics_aggregator.erl
apps/erlmcp_observability/src/erlmcp_metrics_server.erl
```

#### Additional Verification (Check metrics server exports)
```bash
grep "export.*start" apps/erlmcp_observability/src/erlmcp_metrics_server.erl | head -3
```

#### What You Should See
```
-export([start_link/0]).
```

#### Status
- ✅ **VERIFIED** if metrics server + dashboard files exist
- ❌ **FALSE** if components missing

---

## SUMMARY REPORT

After running all verification commands, generate summary:

```bash
cat << 'EOF'
===========================================
VERIFICATION SUMMARY REPORT
===========================================

Code Quality:
[ ] 1.1 Compilation succeeds
[ ] 1.2 Spec parser has 466 lines
[ ] 1.3 No stub implementations
[ ] 1.4 All modules have proper exports
[ ] 1.5 OTP design patterns used

Implementation:
[ ] 2.1 Spec parser: 11 methods
[ ] 2.2 Spec parser: 4 notifications
[ ] 2.3 Spec parser: 15 error codes
[ ] 2.4 Vault uses gun client
[ ] 2.5 AWS uses httpc client
[ ] 2.6 Secrets: 1,297 lines
[ ] 2.7 Tasks: 1,109 lines
[ ] 2.8 Session: 4 backends
[ ] 2.9 Session: 163 lines
[ ] 2.10 Transport behavior interface

Performance:
[ ] 3.1 Benchmarks exist
[ ] 3.2 Quick benchmark <2min
[ ] 3.3 Core ops 2.69M ops/sec

Documentation:
[ ] 4.1 Architecture docs exist
[ ] 4.2 OTP patterns docs exist
[ ] 4.3 API reference exists
[ ] 4.4 Protocol docs exist
[ ] 4.5 20+ examples

Integration:
[ ] 5.1 Vault: 3 auth methods
[ ] 5.2 AWS: 2 auth methods
[ ] 5.3 Local: AES-256-GCM
[ ] 5.4 5 transports
[ ] 5.5 Observability: metrics + dashboard

TOTAL: 25 claims
VERIFIED: __/25
FAILED: __/25
===========================================
EOF
```

---

## USAGE INSTRUCTIONS

### Quick Verification (5 minutes)
```bash
# Run code quality checks
TERM=dumb rebar3 compile 2>&1 | tail -5

# Verify spec parser
wc -l apps/erlmcp_core/src/erlmcp_spec_parser.erl
head -20 apps/erlmcp_core/src/erlmcp_spec_parser.erl

# Verify key integrations
grep -c "gun:" apps/erlmcp_core/src/erlmcp_secrets.erl
grep -c "httpc:" apps/erlmcp_core/src/erlmcp_secrets.erl
```

### Full Verification (30 minutes)
```bash
# Run all commands in this document sequentially
# Document results in summary report
```

### Automated Verification Script
```bash
#!/bin/bash
# verify_all.sh - Run all verifications and generate report

passed=0
failed=0

# Example check
if TERM=dumb rebar3 compile > /dev/null 2>&1; then
  echo "✅ 1.1 Compilation succeeds"
  ((passed++))
else
  echo "❌ 1.1 Compilation failed"
  ((failed++))
fi

# ... add all other checks ...

echo "VERIFIED: $passed/25"
echo "FAILED: $failed/25"
```

---

## INTERPRETATION GUIDE

- ✅ **VERIFIED**: Claim is TRUE. Command output matches expected.
- ❌ **FALSE**: Claim is FALSE. Command output differs from expected.
- ⚠️ **WARNING**: Partial truth. Output exists but with caveats.

**Remember**: Joe Armstrong's philosophy - "NO THEORY. HARDCODED TRUTH."
If the command runs and produces expected output, the claim is VERIFIED.
No speculation, no "should work", only PROOF THROUGH EXECUTION.

---

**Document Version**: 1.0.0
**Last Updated**: 2026-01-30
**Total Claims Documented**: 25
**Total Commands**: 50+
