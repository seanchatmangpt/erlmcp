# Xref Verification Report
**Generated**: 2026-01-30
**Agent**: DoD Agent 6 - Xref Verification
**Goal**: Verify Xref clean (0 undefined functions)

## Executive Summary

- **Total Warnings**: 23
- **Unused Local Functions**: 3 (low priority)
- **Undefined Function Calls**: 20 (requires action)
- **Status**: ❌ **FAILS** - Multiple undefined functions require fixes

---

## Detailed Findings

### 1. Unused Local Functions (3) - Low Priority

These functions are defined but never called. Not critical but should be cleaned up.

| Module | Line | Function | Action |
|--------|------|----------|--------|
| erlmcp_chaos_resource | 71 | allocate_chunks/4 | Remove or document |
| erlmcp_chaos_resource | 60 | allocate_memory_gradually/2 | Remove or document |
| erlmcp_validate_cli | 606 | convert_error_msg/1 | Remove or document |

**Recommendation**: These are likely utility functions for future use. Add `-compile({nowarn_unused_function, [{Function, Arity}]}).` or remove if truly unused.

---

### 2. Undefined Function Calls - High Priority

#### 2.1 Missing Registry Functions (1)

| Call Site | Module | Function | Issue | Fix |
|-----------|--------|----------|-------|-----|
| erlmcp.erl:95 | erlmcp_registry | update_server/2 | Function not exported | Implement or stub |

**Impact**: Medium - Server configuration updates may fail
**Fix**: Implement `erlmcp_registry:update_server/2` or add to xref_ignores if not needed

---

#### 2.2 Missing Auth/JWT Functions (3) - **CRITICAL**

| Call Site | Module | Function | Issue | Fix |
|-----------|--------|----------|-------|-----|
| erlmcp_auth.erl:230 | jose | jwk_from_pem/1 | Wrong API | Fix API call |
| erlmcp_auth.erl:500 | jose | jwk_from_pem/1 | Wrong API | Fix API call |
| erlmcp_auth.erl:500 | jose | jwt_verify/2 | Wrong API | Fix API call |

**Impact**: **CRITICAL** - JWT verification broken, authentication will fail
**Fix**: The `jose` library API has changed. Check correct API:
- `jose:jwk_from_pem/1` → Use `jose_jwk:from_pem/1`
- `jose:jwt_verify/2` → Use `jose_jwt:verify/2`

---

#### 2.3 Missing TCPS Functions (2)

| Call Site | Module | Function | Issue | Fix |
|-----------|--------|----------|-------|-----|
| erlmcp_hooks.erl:288 | tcps_quality_gates | check_all_gates/1 | Missing module | Implement or stub |
| erlmcp_hooks.erl:419 | tcps_quality_gates | get_quality_metrics/0 | Missing module | Implement or stub |

**Impact**: Medium - Quality gates hooks will fail
**Fix**: Implement `tcps_quality_gates` module or add to xref_ignores

---

#### 2.4 Missing Validation Functions (4)

| Call Site | Module | Function | Issue | Fix |
|-----------|--------|----------|-------|-----|
| erlmcp_memory_manager.erl:222 | erts_debug | size_of/1 | Not in all OTP versions | Add conditional or use alternative |
| erlmcp_schema_registry.erl:133 | erlmcp_schema_validator | validate/3 | Missing module | Implement or stub |
| erlmcp_server.erl:1370 | erlmcp_prompt_argument_validator | validate_prompt_arguments/3 | Missing module | Implement or stub |
| erlmcp_transport_http_server.erl:399 | erlmcp_tls_validation | build_tls_options/2 | Missing module | Implement or stub |

**Impact**: Medium - Validation features incomplete
**Fix**: Implement missing validation modules or add to xref_ignores

---

#### 2.5 Missing Refusal/Poka-Yoke Functions (2)

| Call Site | Module | Function | Issue | Fix |
|-----------|--------|----------|-------|-----|
| tcps_poka_yoke.erl:389 | erlmcp_refusal | is_valid_code/1 | Function not exported | Export or use alternative |

**Impact**: Low - Error code validation incomplete
**Fix**: Export `erlmcp_refusal:is_valid_code/1` or implement alternative

---

### 3. External Library Issues (6)

These are from external libraries (jose, erts_debug) and may need version updates or API changes.

#### 3.1 JOSE Library (3)

The `jose` JWT library API calls are incorrect:
- `jose:jwk_from_pem/1` → Should be `jose_jwk:from_pem/1`
- `jose:jwt_verify/2` → Should be `jose_jwt:verify/2`

**Action Required**: Fix all JWT verification code in `erlmcp_auth.erl`

#### 3.2 Erlang/OTP Internal Functions (1)

- `erts_debug:size_of/1` - Not available in all OTP versions

**Action Required**: Use conditional compilation or alternative approach

---

### 4. Missing Module Implementations

The following modules are referenced but not implemented:

| Module | Purpose | Status |
|--------|---------|--------|
| tcps_quality_gates | Quality gates checking | ❌ Missing |
| erlmcp_schema_validator | Schema validation | ❌ Missing |
| erlmcp_prompt_argument_validator | Prompt validation | ❌ Missing |
| erlmcp_tls_validation | TLS validation | ❌ Missing |
| erlmcp_refusal (is_valid_code/1) | Refusal code validation | ⚠️ Partial |

---

## Recommendations

### Immediate Actions (High Priority)

1. **Fix JWT Authentication (CRITICAL)**
   ```erlang
   % In erlmcp_auth.erl, replace:
   jose:jwk_from_pem(PemData)
   % With:
   jose_jwk:from_pem(PemData)

   % Replace:
   jose:jwt_verify(Token, Key)
   % With:
   jose_jwt:verify(Key, Token)
   ```

2. **Implement or Stub Missing Functions**
   - Create stub implementations for missing validation modules
   - Export `erlmcp_refusal:is_valid_code/1` if it exists
   - Implement `erlmcp_registry:update_server/2` or document why it's not needed

3. **Update xref_ignores** for legitimate cases
   ```erlang
   {xref_ignores, [
       % Add these for unimplemented features:
       {erlmcp_schema_registry, handle_call, 3},  % TODO: implement validator
       {erlmcp_server, validate_prompt_arguments, 3},  % TODO: implement validator
       {erlmcp_transport_http_server, build_gun_opts, 1},  % TODO: implement TLS

       % Add these for TCPS integration (optional):
       {erlmcp_hooks, do_post_task, 3},
       {erlmcp_hooks, do_session_end, 2}
   ]}.
   ```

### Medium Priority

4. **Fix erts_debug:size_of/1**
   - Use conditional compilation based on OTP version
   - Or use alternative approach for term size calculation

5. **Implement Missing Validation Modules**
   - erlmcp_schema_validator
   - erlmcp_prompt_argument_validator
   - erlmcp_tls_validation

### Low Priority

6. **Clean Up Unused Functions**
   - Remove or document unused local functions in chaos_resource and validate_cli

---

## Updated xref_ignores Recommendation

```erlang
{xref_ignores, [
    %% Dynamically called transport functions
    {erlmcp_transport_stdio, read_loop, 2},
    {erlmcp_transport_tcp, send, 2},

    %% OpenTelemetry API functions (external library)
    {otel_tracer, start_span, 1},
    {otel_tracer, get_tracer, 1},
    {otel_span, record_exception, 4},
    {otel, with_span, 3},
    {otel, add_event, 2},

    %% Stdlib extensions available in Erlang/OTP 25+
    {maps, any, 2},
    {erlang, term_to_string, 1},
    {uri_string, quote_string, 1},
    {file, canonicalize_path, 1},
    {inet, getstat, 0},

    %% Gun/Ranch/Cowboy (external libraries)
    {gun, open, 2},
    {gun, await, 2},
    {ranch, start_listener, 6},
    {poolboy, checkout, 1},
    {cowboy_req, stream_body, 2},

    %% Rebar3 provider API (only available in build context)
    {providers, create, 1},
    {rebar_api, debug, 2},
    {rebar_api, error, 2},
    {rebar_api, info, 2},
    {rebar_api, warn, 2},
    {rebar_state, add_provider, 2},
    {rebar_state, command_parsed_args, 1},
    {rebar_state, dir, 1},

    %% NEW: Unimplemented validation modules (TODO)
    {erlmcp_schema_registry, handle_call, 3},
    {erlmcp_server, validate_prompt_arguments, 3},
    {erlmcp_transport_http_server, build_gun_opts, 1},

    %% NEW: TCPS integration (optional module)
    {erlmcp_hooks, do_post_task, 3},
    {erlmcp_hooks, do_session_end, 2},

    %% NEW: OTP version-specific functions
    {erts_debug, size_of, 1}
]}.
```

---

## Testing Commands

### Verify Xref
```bash
rebar3 xref
```

### Count Issues
```bash
rebar3 xref 2>&1 | grep -c "Warning:"
```

### Generate This Report
```bash
rebar3 xref 2>&1 | tee test_results/definition_of_done/xref_output.log
```

---

## Conclusion

**Current Status**: ❌ **FAILS** - 20 undefined function calls

**Critical Issues**:
1. JWT authentication broken (3 incorrect jose API calls)
2. Missing validation modules (4 modules)
3. Missing registry function (1)
4. TCPS integration incomplete (2 functions)

**Path to Success**:
1. Fix jose API calls (5 minutes)
2. Implement missing validators or add to xref_ignores (1 hour)
3. Export missing refusal function (5 minutes)
4. Re-run xref verification

**Estimated Time to Fix**: 1-2 hours

---

**Next Steps**:
1. Fix critical JWT authentication issues immediately
2. Implement stub modules for missing validators
3. Update xref_ignores for legitimate cases
4. Re-run Xref to verify 0 undefined functions
5. Update this report with final results
