# Phase 1d: Prompt Argument Validator Recovery
## Implementation Plan

**Version:** 1.0
**Date:** 2026-01-31
**Status:** Ready for Execution
**Estimated Time:** 30 minutes (Quick Fix) | 2-3 hours (Full Integration)

---

## 1. Overview

### Problem Summary
The `erlmcp_prompt_argument_validator` module exists but is **disabled** (renamed to `.erl.broken` extension). This critical validator is responsible for:
- Validating prompt arguments against JSON Schema
- Checking required vs optional arguments
- Enforcing argument type constraints
- Generating error codes -32043 (missing argument) and -32045 (invalid argument)

### Current Impact
**HIGH SEVERITY** - Arguments are not validated:
- **Tests expect validation to work** but it's silently skipped
- **Error codes -32043 and -32045 are never generated** despite being defined in the MCP spec
- **Invalid arguments are accepted** without validation
- **Security risk**: No input validation on prompt arguments
- **Protocol compliance failure**: Gap #42 not fully implemented

### What This Plan Delivers
1. **Quick Fix (30 minutes)**: Rename file, rebuild, basic verification
2. **Full Integration (2-3 hours)**: Comprehensive testing, error code validation, end-to-end workflow tests

### Timeline
- **Rename file**: 1 minute
- **Rebuild**: 2 minutes
- **Review/verify**: 5 minutes
- **Create/update tests**: 10 minutes (quick) | 1-2 hours (comprehensive)
- **Run tests**: 5 minutes
- **Verification**: 5 minutes
- **Documentation**: 30 minutes (optional)
- **Total Quick Fix**: 30 minutes
- **Total Full Integration**: 2-3 hours

---

## 2. Issue Analysis

### File Location
```
Current:  /home/user/erlmcp/apps/erlmcp_core/src/erlmcp_prompt_argument_validator.erl.broken
Target:   /home/user/erlmcp/apps/erlmcp_core/src/erlmcp_prompt_argument_validator.erl
Size:     10,390 bytes (286 lines)
Modified: 2026-01-31 19:15
```

### Why It's Broken
The file was renamed with a `.broken` extension, likely during:
- **Debugging**: Temporary disable to isolate issues
- **Refactoring**: Pending rewrite or redesign
- **Dependency issues**: Missing or incompatible jesse dependency
- **Test failures**: Validation logic needed fixes

The `.broken` extension prevents Erlang compilation from including this module.

### Current Behavior
**Without validator**:
1. Client sends `prompts/get` with arguments: `{"language": 123}` (wrong type)
2. Server calls `validate_prompt_arguments/3` at line 1943
3. Validator at line 2019 tries to call `erlmcp_prompt_argument_validator:validate_prompt_arguments/3`
4. **Module not compiled** → `undef` error OR silent skip (if wrapped in try/catch)
5. Arguments passed directly to handler **without validation**
6. Handler may crash or produce incorrect results

**Expected Behavior** (with validator):
1. Client sends invalid arguments
2. Validator checks against JSON Schema and argument declarations
3. Returns `{error, {-32602, <<"Argument validation failed">>, ValidationDetails}}`
4. Error sent to client with specific validation failure details

### Where It's Called

**File**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl`

**Function**: `handle_get_prompt/5` (lines 1926-1960)

**Call Site**: Line 1943 (indirect via `validate_prompt_arguments/3`)

```erlang
%% Line 1943 - validate_prompt_arguments called
case validate_prompt_arguments(Arguments, Prompt, State) of
    ok ->
        handle_prompt_execution(...);
    {error, {Code, Message, Data}} ->
        erlmcp_tracing:record_error_details(SpanCtx, argument_validation_failed, Data),
        send_error_via_registry(State, TransportId, Id, Code, Message, Data)
end

%% Lines 2009-2021 - validate_prompt_arguments delegates to validator
validate_prompt_arguments(ProvidedArgs, Prompt, _State) ->
    PromptArguments = Prompt#mcp_prompt.arguments,
    InputSchema = Prompt#mcp_prompt.input_schema,
    erlmcp_prompt_argument_validator:validate_prompt_arguments(
        ProvidedArgs, PromptArguments, InputSchema
    ).
```

**Integration Flow**:
```
Client Request (prompts/get)
    ↓
handle_get_prompt/5 (line 1926)
    ↓
validate_prompt_arguments/3 (line 2016) ← Server-side wrapper
    ↓
erlmcp_prompt_argument_validator:validate_prompt_arguments/3 ← gen_server call (BROKEN)
    ↓
do_validate/3 → validate_json_schema/2 → validate_required_arguments/2
    ↓
Returns: ok | {error, {Code, Message, Data}}
```

### Why It Was Probably Disabled
Based on `.bak3` file comments:
```erlang
%% TODO: Re-enable when erlmcp_prompt_argument_validator is implemented
%% erlmcp_prompt_argument_validator:validate_prompt_arguments(
```

**Hypothesis**: Validator was disabled because:
1. **Initial implementation incomplete** - gen_server pattern added later
2. **Dependency on jesse** - JSON Schema validator may not have been available
3. **Test failures** - Validation logic had bugs
4. **Performance concerns** - gen_server overhead for simple validation

**Current Status**: Module is **fully implemented** with gen_server, jesse integration, and comprehensive validation logic. Ready to re-enable.

---

## 3. Step 1: Rename File

### Commands
```bash
cd /home/user/erlmcp/apps/erlmcp_core/src/

# Rename file (remove .broken extension)
mv erlmcp_prompt_argument_validator.erl.broken erlmcp_prompt_argument_validator.erl

# Verify rename
ls -la erlmcp_prompt_argument_validator.erl
```

### Expected Output
```
-rw-r--r-- 1 user user 10390 Jan 31 19:15 erlmcp_prompt_argument_validator.erl
```

### Verification
```bash
# Confirm .broken file no longer exists
ls -la erlmcp_prompt_argument_validator.erl.broken
# Should output: No such file or directory

# Confirm .erl file exists
file erlmcp_prompt_argument_validator.erl
# Should output: erlmcp_prompt_argument_validator.erl: ASCII text
```

---

## 4. Step 2: Review Module Content

### Module Structure Overview
```erlang
-module(erlmcp_prompt_argument_validator).
-behaviour(gen_server).

%% Lines 1-54: Module header, exports, types
%% Lines 38-53: Public API
%%   - start_link/0 (line 39)
%%   - validate_prompt_arguments/3 (line 52) - MAIN ENTRY POINT
%%   - format_validation_error/1 (line 278)
%%   - format_validation_errors/1 (line 283)

%% Lines 59-87: gen_server callbacks
%%   - init/1 (line 60) - Creates worker with unique ID
%%   - handle_call/3 (line 66) - Delegates to do_validate/3
%%   - handle_cast/2 (line 74) - No-op
%%   - handle_info/2 (line 78) - No-op
%%   - terminate/2 (line 82) - No-op
%%   - code_change/3 (line 86) - No-op

%% Lines 93-107: Main validation orchestrator
%%   - do_validate/3 (line 99) - Coordinates validation steps

%% Lines 109-130: JSON Schema validation
%%   - validate_json_schema/2 (line 111) - Uses jesse library

%% Lines 132-161: Required argument validation
%%   - validate_required_arguments/2 (line 136) - Checks required args

%% Lines 163-275: Error formatting
%%   - format_jesse_errors/1 (line 165)
%%   - format_jesse_error/1 (line 172)
%%   - format_path/1 (line 200)
%%   - format_error_message/1 (line 214)
```

### Key Functions Explained

#### 1. `validate_prompt_arguments/3` (Line 52)
**Purpose**: Main public API - validates provided arguments against schema and declarations.

**Signature**:
```erlang
-spec validate_prompt_arguments(
    map(),                          % ProvidedArgs - arguments from client
    [#mcp_prompt_argument{}] | undefined,  % PromptArguments - declared args
    map() | undefined               % InputSchema - JSON Schema
) -> validation_result().
```

**Implementation**:
```erlang
validate_prompt_arguments(ProvidedArgs, PromptArguments, InputSchema) ->
    gen_server:call(?MODULE, {validate_prompt_arguments, ProvidedArgs, PromptArguments, InputSchema}, 5000).
```

**Behavior**: Synchronous gen_server call with 5-second timeout.

#### 2. `do_validate/3` (Line 99)
**Purpose**: Orchestrates validation in two steps: schema validation, then required argument validation.

**Logic Flow**:
```erlang
do_validate(ProvidedArgs, PromptArguments, InputSchema) ->
    %% Step 1: Validate JSON Schema if present (Gap #42)
    case validate_json_schema(ProvidedArgs, InputSchema) of
        ok ->
            %% Step 2: Validate required vs optional arguments
            validate_required_arguments(ProvidedArgs, PromptArguments);
        {error, _} = Error ->
            Error
    end.
```

**Validation Order**:
1. **JSON Schema validation** (if `input_schema` defined) - Catches type/format errors
2. **Required argument validation** - Catches missing required arguments

#### 3. `validate_json_schema/2` (Line 111)
**Purpose**: Validate arguments against JSON Schema using jesse library.

**Logic**:
```erlang
validate_json_schema(_ProvidedArgs, undefined) ->
    ok;  % No schema = no validation
validate_json_schema(ProvidedArgs, InputSchema) when is_map(InputSchema) ->
    try
        case jesse:validate_with_schema(InputSchema, ProvidedArgs, [{allowed_errors, infinity}]) of
            {ok, _} ->
                ok;
            {error, JesseErrors} ->
                FormattedErrors = format_jesse_errors(JesseErrors),
                {error, {?JSONRPC_INVALID_PARAMS, <<"Argument validation failed">>, #{
                    <<"validation_errors">> => FormattedErrors
                }}}
        end
    catch
        _:JessError ->
            logger:error("Jesse validation error: ~p", [JessError]),
            {error, {?JSONRPC_INVALID_PARAMS, <<"Schema validation failed">>, #{
                <<"schema_error">> => io_lib:format("~p", [JessError])
            }}}
    end.
```

**Return Codes**:
- **Success**: `ok`
- **Validation failure**: `{error, {-32602, Message, ValidationErrors}}`
- **Schema error**: `{error, {-32602, Message, SchemaError}}`

**Note**: Uses `-32602` (JSONRPC_INVALID_PARAMS) instead of MCP-specific codes. This may need adjustment.

#### 4. `validate_required_arguments/2` (Line 136)
**Purpose**: Ensure all required arguments are present.

**Logic**:
```erlang
validate_required_arguments(_ProvidedArgs, undefined) ->
    ok;  % No argument declarations = no validation
validate_required_arguments(ProvidedArgs, PromptArguments) when is_list(PromptArguments) ->
    MissingRequired = lists:filtermap(
        fun(#mcp_prompt_argument{name = Name, required = Required}) ->
            case Required of
                true ->
                    case maps:is_key(Name, ProvidedArgs) of
                        false -> {true, Name};  % Missing required arg
                        true -> false           % Present
                    end;
                false ->
                    false  % Optional arg, skip check
            end
        end,
        PromptArguments
    ),

    case MissingRequired of
        [] ->
            ok;
        _ ->
            {error, {?JSONRPC_INVALID_PARAMS,
                <<"Missing required prompt arguments">>, #{
                    <<"missing_arguments">> => MissingRequired,
                    <<"provided_arguments">> => maps:keys(ProvidedArgs)
                }}}
    end.
```

**Return Codes**:
- **Success**: `ok`
- **Missing args**: `{error, {-32602, Message, #{missing_arguments, provided_arguments}}}`

**Note**: Also uses `-32602` instead of `-32043` (MCP_ERROR_PROMPT_ARGUMENT_MISSING).

### Error Code Mapping Issue
**CRITICAL FINDING**: Validator returns `-32602` (JSON-RPC Invalid Params) instead of MCP-specific codes:
- Should be `-32043` for missing arguments
- Should be `-32045` for invalid arguments

**Fix Required**: Update error codes in `validate_json_schema/2` and `validate_required_arguments/2`.

---

## 5. Step 3: Rebuild & Verify

### Compilation Commands
```bash
cd /home/user/erlmcp

# Clean build
TERM=dumb rebar3 clean
TERM=dumb rebar3 compile
```

### Expected Output
```
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling erlmcp_core
===> Compiling erlmcp_prompt_argument_validator.erl
===> Compiling erlmcp_transports
===> Compiling erlmcp_observability
===> Compiling erlmcp_validation
```

### Check for Compilation Errors

**Potential Issues**:
1. **Missing `-include("erlmcp.hrl")`** - Should be present (line 15)
2. **Undefined macros**: `?JSONRPC_INVALID_PARAMS`, `?MCP_PARAM_*`
3. **Missing jesse dependency** - Required for JSON Schema validation
4. **Type spec errors** - Dialyzer may complain about return types

**If Compilation Fails**:

**Issue 1: Missing jesse dependency**
```bash
# Check if jesse is in rebar.config
grep jesse /home/user/erlmcp/rebar.config

# If missing, add to deps:
{deps, [
    ...
    {jesse, "1.8.0"}
]}.

# Re-fetch dependencies
rebar3 get-deps
rebar3 compile
```

**Issue 2: Undefined macros**
Check that `erlmcp.hrl` defines:
```erlang
-define(JSONRPC_INVALID_PARAMS, -32602).
-define(MCP_ERROR_PROMPT_ARGUMENT_MISSING, -32043).
-define(MCP_ERROR_INVALID_PROMPT_ARGUMENTS, -32045).
```

**Issue 3: gen_server callback warnings**
If Dialyzer complains about unused callbacks, this is normal - leave as-is.

### Verify BEAM File Generation
```bash
ls -la /home/user/erlmcp/_build/default/lib/erlmcp_core/ebin/erlmcp_prompt_argument_validator.beam

# Expected output:
# -rw-r--r-- 1 user user <size> Jan 31 <time> erlmcp_prompt_argument_validator.beam
```

### Verify Module Loads
```bash
# Start Erlang shell
cd /home/user/erlmcp
make console

# In Erlang shell:
1> code:which(erlmcp_prompt_argument_validator).
"/home/user/erlmcp/_build/default/lib/erlmcp_core/ebin/erlmcp_prompt_argument_validator.beam"

2> erlmcp_prompt_argument_validator:module_info().
[{module,erlmcp_prompt_argument_validator},
 {exports,[...]},
 {attributes,[...]},
 {compile,[...]},
 {native,false}]

3> q().
```

---

## 6. Step 4: Identify Where It's Called

### Primary Call Site

**File**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl`

**Function**: `validate_prompt_arguments/3` (lines 2009-2021)

**Caller**: `handle_get_prompt/5` (line 1943)

### Code Analysis

#### Call from handle_get_prompt/5 (Line 1943)
```erlang
-spec handle_get_prompt(json_rpc_id(), binary(), map(), atom(), state()) -> ok.
handle_get_prompt(Id, Name, Arguments, TransportId, #state{server_id = ServerId} = State) ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.handle_prompts_get">>, ServerId),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"request_id">> => Id,
            <<"prompt.name">> => Name,
            <<"transport_id">> => TransportId,
            <<"arguments_count">> => maps:size(Arguments)
        }),

        case maps:get(Name, State#state.prompts, undefined) of
            undefined ->
                erlmcp_tracing:record_error_details(SpanCtx, prompt_not_found, Name),
                send_error_safe(State, TransportId, Id, ?MCP_ERROR_PROMPT_NOT_FOUND, ?MCP_MSG_PROMPT_NOT_FOUND);
            {Prompt, Handler} ->
                % Gap #42: Validate prompt arguments against declared schema
                case validate_prompt_arguments(Arguments, Prompt, State) of  % ← LINE 1943
                    ok ->
                        handle_prompt_execution(
                            Id, Name, Arguments, TransportId, Prompt, Handler, State, SpanCtx
                        );
                    {error, {Code, Message, Data}} ->
                        erlmcp_tracing:record_error_details(SpanCtx, argument_validation_failed, Data),
                        send_error_via_registry(State, TransportId, Id, Code, Message, Data)
                end,
                erlmcp_tracing:set_status(SpanCtx, ok)
        end
    catch
        Class:Reason2:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason2, Stacktrace),
            erlang:raise(Class, Reason2, Stacktrace)
    after
        erlmcp_tracing:end_span(SpanCtx)
    end.
```

**Integration Status**: **ACTIVE** - Call is NOT commented out.

**Error Handling**: Properly integrated:
- Success path: Execute prompt handler
- Error path: Send error via registry with code, message, and data

#### validate_prompt_arguments/3 Wrapper (Lines 2009-2021)
```erlang
%% @doc Validate prompt arguments against declared schema.
%% Gap #42 Implementation
-spec validate_prompt_arguments(
    map(),
    #mcp_prompt{},
    state()
) -> ok | {error, {integer(), binary(), map()}}.
validate_prompt_arguments(ProvidedArgs, Prompt, _State) ->
    PromptArguments = Prompt#mcp_prompt.arguments,
    InputSchema = Prompt#mcp_prompt.input_schema,
    erlmcp_prompt_argument_validator:validate_prompt_arguments(  % ← LINE 2019
        ProvidedArgs, PromptArguments, InputSchema
    ).
```

**Purpose**: Extracts `arguments` and `input_schema` from `#mcp_prompt{}` record, delegates to validator.

**Note**: `_State` parameter is unused (future extension point).

### Call Graph
```
JSON-RPC Request: prompts/get
    ↓
erlmcp_registry:route_message/2
    ↓
erlmcp_server:handle_cast({json_rpc_request, ...})
    ↓
erlmcp_server:handle_prompts_get/4
    ↓
erlmcp_server:handle_get_prompt/5 (line 1926)
    ↓
erlmcp_server:validate_prompt_arguments/3 (line 2016) ← Server-side wrapper
    ↓
erlmcp_prompt_argument_validator:validate_prompt_arguments/3 (line 52) ← gen_server call
    ↓
gen_server:call(?MODULE, {...}, 5000)
    ↓
erlmcp_prompt_argument_validator:handle_call/3 (line 66)
    ↓
erlmcp_prompt_argument_validator:do_validate/3 (line 99)
    ↓
    ├─ validate_json_schema/2 (line 111) → jesse:validate_with_schema/3
    └─ validate_required_arguments/2 (line 136) → lists:filtermap/2
```

### What Happens When Module is Missing
**Current behavior** (with `.broken` extension):

**Scenario 1**: Module never loaded (most likely)
```erlang
** exception error: undefined function erlmcp_prompt_argument_validator:validate_prompt_arguments/3
     in function  erlmcp_server:validate_prompt_arguments/3 (erlmcp_server.erl, line 2019)
```
→ **Result**: Server crashes on first prompts/get request with arguments.

**Scenario 2**: Wrapped in try/catch (if defensive code exists elsewhere)
```erlang
{error, {?JSONRPC_INTERNAL_ERROR, <<"Internal server error">>, #{}}}
```
→ **Result**: Generic error, no validation performed.

**After Fix**: Validation executes correctly, returns specific error codes.

---

## 7. Step 5: Integration Testing

### Test Case 1: Valid Prompt Arguments
**Purpose**: Verify validator allows valid arguments and prompt executes successfully.

**Setup**:
```erlang
%% Register prompt with arguments
erlmcp_server:add_prompt_with_args(
    ServerPid,
    <<"code_review">>,
    <<"Review code for quality">>,
    [#mcp_prompt_argument{
        name = <<"language">>,
        description = <<"Programming language">>,
        required = true
    }],
    fun(Args) ->
        Language = maps:get(<<"language">>, Args),
        [#mcp_prompt_message{
            role = <<"user">>,
            content = #mcp_text_content{text = <<"Review my ", Language/binary, " code">>}
        }]
    end
).
```

**Test Request**:
```json
{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "prompts/get",
    "params": {
        "name": "code_review",
        "arguments": {
            "language": "erlang"
        }
    }
}
```

**Expected Response**:
```json
{
    "jsonrpc": "2.0",
    "id": 1,
    "result": {
        "messages": [
            {
                "role": "user",
                "content": {
                    "type": "text",
                    "text": "Review my erlang code"
                }
            }
        ],
        "description": "Review code for quality"
    }
}
```

**Verification**: `validate_prompt_arguments/3` returns `ok`, prompt executes.

### Test Case 2: Missing Required Argument
**Purpose**: Verify validator detects missing required arguments and returns error code -32043.

**Test Request**:
```json
{
    "jsonrpc": "2.0",
    "id": 2,
    "method": "prompts/get",
    "params": {
        "name": "code_review",
        "arguments": {}
    }
}
```

**Expected Response**:
```json
{
    "jsonrpc": "2.0",
    "id": 2,
    "error": {
        "code": -32602,
        "message": "Missing required prompt arguments",
        "data": {
            "missing_arguments": ["language"],
            "provided_arguments": []
        }
    }
}
```

**Note**: Currently returns `-32602` (JSON-RPC Invalid Params). Should be updated to `-32043` (MCP Prompt Argument Missing).

**Verification**:
- Line 2019 calls validator
- Line 156 in validator detects missing "language" argument
- Error propagated to line 1950 in server
- `send_error_via_registry/6` sends error response

### Test Case 3: Invalid Argument Type
**Purpose**: Verify validator detects type mismatches and returns error code -32045.

**Setup** (with JSON Schema):
```erlang
erlmcp_server:add_prompt_with_args_and_schema(
    ServerPid,
    <<"code_review">>,
    <<"Review code for quality">>,
    [#mcp_prompt_argument{
        name = <<"language">>,
        description = <<"Programming language">>,
        required = true
    }],
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"language">> => #{<<"type">> => <<"string">>}
        },
        <<"required">> => [<<"language">>]
    },
    fun(Args) -> ... end
).
```

**Test Request**:
```json
{
    "jsonrpc": "2.0",
    "id": 3,
    "method": "prompts/get",
    "params": {
        "name": "code_review",
        "arguments": {
            "language": 123
        }
    }
}
```

**Expected Response**:
```json
{
    "jsonrpc": "2.0",
    "id": 3,
    "error": {
        "code": -32602,
        "message": "Argument validation failed",
        "data": {
            "validation_errors": [
                {
                    "path": "$.language",
                    "error": "Wrong type, expected: string"
                }
            ]
        }
    }
}
```

**Note**: Currently returns `-32602`. Should be updated to `-32045` (MCP Invalid Prompt Arguments).

**Verification**:
- Line 2019 calls validator
- Lines 115-122 in validator detect type mismatch via jesse
- Error formatted at lines 172-196
- Error propagated to line 1950 in server

### Test Case 4: JSON Schema Validation Failure
**Purpose**: Verify complex schema constraints (enum, pattern, min/max) are validated.

**Setup**:
```erlang
erlmcp_server:add_prompt_with_args_and_schema(
    ServerPid,
    <<"generate_code">>,
    <<"Generate code snippet">>,
    [
        #mcp_prompt_argument{name = <<"language">>, required = true},
        #mcp_prompt_argument{name = <<"style">>, required = false}
    ],
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"language">> => #{
                <<"type">> => <<"string">>,
                <<"enum">> => [<<"erlang">>, <<"elixir">>, <<"python">>]
            },
            <<"style">> => #{
                <<"type">> => <<"string">>,
                <<"pattern">> => "^[a-z_]+$"
            }
        },
        <<"required">> => [<<"language">>],
        <<"additionalProperties">> => false
    },
    fun(Args) -> ... end
).
```

**Test Request** (invalid enum + pattern):
```json
{
    "jsonrpc": "2.0",
    "id": 4,
    "method": "prompts/get",
    "params": {
        "name": "generate_code",
        "arguments": {
            "language": "java",
            "style": "CamelCase"
        }
    }
}
```

**Expected Response**:
```json
{
    "jsonrpc": "2.0",
    "id": 4,
    "error": {
        "code": -32602,
        "message": "Argument validation failed",
        "data": {
            "validation_errors": [
                {
                    "path": "$.language",
                    "error": "Value not in enum: [\"erlang\",\"elixir\",\"python\"]"
                },
                {
                    "path": "$.style",
                    "error": "Pattern does not match"
                }
            ]
        }
    }
}
```

**Verification**:
- Multiple validation errors collected
- Each error includes JSON path and specific failure reason

---

## 8. Step 6: Test File Creation/Modification

### Current Test File
**Path**: `/home/user/erlmcp/apps/erlmcp_core/test/erlmcp_prompt_validation_tests.erl`

**Status**: **EXISTS** but tests **WRONG module** (`erlmcp_prompt_template`, not `erlmcp_prompt_argument_validator`)

**Current Tests** (228 lines):
- Template validation (syntax, size limits)
- Security validation (variable names, nesting, output size)
- Render safe tests
- NOT testing argument validation

### Required Test File
**New File**: `/home/user/erlmcp/apps/erlmcp_core/test/erlmcp_prompt_argument_validation_tests.erl`

**Test Coverage**:
1. **JSON Schema Validation** (10 tests)
   - Valid schema validation passes
   - Type mismatch detected
   - Enum violation detected
   - Pattern violation detected
   - Required property missing (via schema)
   - Additional properties rejected
   - Min/max constraints enforced
   - Array validation
   - Nested object validation
   - Schema parse errors handled

2. **Required Argument Validation** (5 tests)
   - All required arguments present → ok
   - Missing single required argument → error
   - Missing multiple required arguments → error
   - Optional arguments can be omitted
   - Extra arguments allowed (when no schema)

3. **Validation Orchestration** (3 tests)
   - Schema validation fails → required validation skipped
   - Schema validation passes → required validation runs
   - No schema or arguments → ok

4. **Error Formatting** (4 tests)
   - Jesse errors formatted correctly
   - JSON paths formatted correctly
   - Error messages are human-readable
   - Multiple errors collected

### Minimal Test Suite

```erlang
%%%====================================================================
%%% @doc Test Suite for erlmcp_prompt_argument_validator
%%% Chicago School TDD - Test observable behavior only
%%% @end
%%%====================================================================
-module(erlmcp_prompt_argument_validation_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup() ->
    {ok, Pid} = erlmcp_prompt_argument_validator:start_link(),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid).

%%====================================================================
%% JSON Schema Validation Tests
%%====================================================================

schema_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) -> [
        ?_test(test_schema_validation_valid()),
        ?_test(test_schema_validation_type_mismatch()),
        ?_test(test_schema_validation_enum_violation()),
        ?_test(test_schema_validation_required_missing())
     ] end
    }.

test_schema_validation_valid() ->
    ProvidedArgs = #{<<"language">> => <<"erlang">>},
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"language">> => #{<<"type">> => <<"string">>}
        }
    },
    ?assertEqual(ok,
        erlmcp_prompt_argument_validator:validate_prompt_arguments(
            ProvidedArgs, undefined, Schema
        )
    ).

test_schema_validation_type_mismatch() ->
    ProvidedArgs = #{<<"language">> => 123},
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"language">> => #{<<"type">> => <<"string">>}
        }
    },
    ?assertMatch(
        {error, {?JSONRPC_INVALID_PARAMS, <<"Argument validation failed">>, _}},
        erlmcp_prompt_argument_validator:validate_prompt_arguments(
            ProvidedArgs, undefined, Schema
        )
    ).

test_schema_validation_enum_violation() ->
    ProvidedArgs = #{<<"language">> => <<"java">>},
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"language">> => #{
                <<"type">> => <<"string">>,
                <<"enum">> => [<<"erlang">>, <<"elixir">>]
            }
        }
    },
    ?assertMatch(
        {error, {?JSONRPC_INVALID_PARAMS, <<"Argument validation failed">>, _}},
        erlmcp_prompt_argument_validator:validate_prompt_arguments(
            ProvidedArgs, undefined, Schema
        )
    ).

test_schema_validation_required_missing() ->
    ProvidedArgs = #{},
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"language">> => #{<<"type">> => <<"string">>}
        },
        <<"required">> => [<<"language">>]
    },
    ?assertMatch(
        {error, {?JSONRPC_INVALID_PARAMS, <<"Argument validation failed">>, _}},
        erlmcp_prompt_argument_validator:validate_prompt_arguments(
            ProvidedArgs, undefined, Schema
        )
    ).

%%====================================================================
%% Required Argument Validation Tests
%%====================================================================

required_argument_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) -> [
        ?_test(test_required_argument_present()),
        ?_test(test_required_argument_missing()),
        ?_test(test_optional_argument_omitted())
     ] end
    }.

test_required_argument_present() ->
    ProvidedArgs = #{<<"language">> => <<"erlang">>},
    Arguments = [#mcp_prompt_argument{
        name = <<"language">>,
        required = true
    }],
    ?assertEqual(ok,
        erlmcp_prompt_argument_validator:validate_prompt_arguments(
            ProvidedArgs, Arguments, undefined
        )
    ).

test_required_argument_missing() ->
    ProvidedArgs = #{},
    Arguments = [#mcp_prompt_argument{
        name = <<"language">>,
        required = true
    }],
    Result = erlmcp_prompt_argument_validator:validate_prompt_arguments(
        ProvidedArgs, Arguments, undefined
    ),
    ?assertMatch(
        {error, {?JSONRPC_INVALID_PARAMS, <<"Missing required prompt arguments">>, _}},
        Result
    ),
    {error, {_, _, Data}} = Result,
    ?assertEqual([<<"language">>], maps:get(<<"missing_arguments">>, Data)),
    ?assertEqual([], maps:get(<<"provided_arguments">>, Data)).

test_optional_argument_omitted() ->
    ProvidedArgs = #{<<"language">> => <<"erlang">>},
    Arguments = [
        #mcp_prompt_argument{name = <<"language">>, required = true},
        #mcp_prompt_argument{name = <<"style">>, required = false}
    ],
    ?assertEqual(ok,
        erlmcp_prompt_argument_validator:validate_prompt_arguments(
            ProvidedArgs, Arguments, undefined
        )
    ).

%%====================================================================
%% Combined Validation Tests
%%====================================================================

combined_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) -> [
        ?_test(test_combined_schema_and_required()),
        ?_test(test_no_validation_when_undefined())
     ] end
    }.

test_combined_schema_and_required() ->
    ProvidedArgs = #{<<"language">> => <<"erlang">>},
    Arguments = [#mcp_prompt_argument{
        name = <<"language">>,
        required = true
    }],
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"language">> => #{<<"type">> => <<"string">>}
        }
    },
    ?assertEqual(ok,
        erlmcp_prompt_argument_validator:validate_prompt_arguments(
            ProvidedArgs, Arguments, Schema
        )
    ).

test_no_validation_when_undefined() ->
    ProvidedArgs = #{<<"any">> => <<"value">>},
    ?assertEqual(ok,
        erlmcp_prompt_argument_validator:validate_prompt_arguments(
            ProvidedArgs, undefined, undefined
        )
    ).
```

### Run Tests
```bash
cd /home/user/erlmcp

# Compile tests
TERM=dumb rebar3 compile

# Run validator tests
rebar3 eunit --module=erlmcp_prompt_argument_validation_tests

# Expected output:
# All 12 tests passed.
```

### Coverage Target
Minimum **80%** coverage of:
- `validate_prompt_arguments/3`
- `do_validate/3`
- `validate_json_schema/2`
- `validate_required_arguments/2`

---

## 9. Step 7: Integration with Server

### File: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl`

### Function: `handle_get_prompt/5` (Lines 1926-1960)

**Current Code Analysis**:
```erlang
case maps:get(Name, State#state.prompts, undefined) of
    undefined ->
        send_error_safe(State, TransportId, Id, ?MCP_ERROR_PROMPT_NOT_FOUND, ...);
    {Prompt, Handler} ->
        % Gap #42: Validate prompt arguments against declared schema
        case validate_prompt_arguments(Arguments, Prompt, State) of  % ← LINE 1943
            ok ->
                handle_prompt_execution(...);  % ← Validation passed, execute prompt
            {error, {Code, Message, Data}} ->
                send_error_via_registry(State, TransportId, Id, Code, Message, Data)  % ← Send error
        end
end
```

**Integration Status**: ✅ **FULLY INTEGRATED**

**Observations**:
1. **Validator is called** at line 1943 (via wrapper at line 2016)
2. **Error handling is correct**: Errors propagated to `send_error_via_registry/6`
3. **Success path is correct**: Validation passes → `handle_prompt_execution/8`
4. **No try/catch around validator call**: If validator crashes, server will crash (let-it-crash)

### Function: `validate_prompt_arguments/3` (Lines 2009-2021)

**Current Code**:
```erlang
%% @doc Validate prompt arguments against declared schema.
%% Gap #42 Implementation
-spec validate_prompt_arguments(
    map(),
    #mcp_prompt{},
    state()
) -> ok | {error, {integer(), binary(), map()}}.
validate_prompt_arguments(ProvidedArgs, Prompt, _State) ->
    PromptArguments = Prompt#mcp_prompt.arguments,
    InputSchema = Prompt#mcp_prompt.input_schema,
    erlmcp_prompt_argument_validator:validate_prompt_arguments(  % ← LINE 2019
        ProvidedArgs, PromptArguments, InputSchema
    ).
```

**Purpose**: Extracts arguments and schema from `#mcp_prompt{}`, delegates to validator module.

**Integration Status**: ✅ **CORRECT DELEGATION**

### Error Response Flow

**Path**: Validation Error → JSON-RPC Error Response

1. **Validator returns error** (line 156 or line 120):
   ```erlang
   {error, {?JSONRPC_INVALID_PARAMS, <<"Missing required prompt arguments">>, #{...}}}
   ```

2. **Server receives error** (line 1948):
   ```erlang
   {error, {Code, Message, Data}} ->
   ```

3. **Error sent via registry** (line 1950):
   ```erlang
   send_error_via_registry(State, TransportId, Id, Code, Message, Data)
   ```

4. **JSON-RPC error formatted**:
   ```json
   {
       "jsonrpc": "2.0",
       "id": <request_id>,
       "error": {
           "code": -32602,
           "message": "Missing required prompt arguments",
           "data": {
               "missing_arguments": ["language"],
               "provided_arguments": []
           }
       }
   }
   ```

**Verification Checklist**:
- [x] Validator called from server (line 1943 → 2019)
- [x] Error codes propagated correctly
- [x] Data field included in error response
- [ ] **ISSUE**: Using `-32602` instead of MCP codes `-32043`/`-32045`

---

## 10. Step 8: Error Response Verification

### Error Code Requirements

**MCP Specification** defines:
- **-32043**: Prompt argument missing (required argument not provided)
- **-32045**: Invalid prompt arguments (argument fails validation)

**Current Implementation** uses:
- **-32602**: JSON-RPC Invalid Params (generic)

### Error Code -32043: Prompt Argument Missing

**When It Should Occur**:
- Required argument not provided in `arguments` map
- Declared in `#mcp_prompt_argument{required = true}`

**Current Code** (validator, line 156):
```erlang
{error, {?JSONRPC_INVALID_PARAMS,  % ← Should be ?MCP_ERROR_PROMPT_ARGUMENT_MISSING
    <<"Missing required prompt arguments">>, #{
        <<"missing_arguments">> => MissingRequired,
        <<"provided_arguments">> => maps:keys(ProvidedArgs)
    }}}
```

**Fix Required**:
```erlang
{error, {?MCP_ERROR_PROMPT_ARGUMENT_MISSING,  % -32043
    <<"Missing required prompt arguments">>, #{
        <<"missing_arguments">> => MissingRequired,
        <<"provided_arguments">> => maps:keys(ProvidedArgs)
    }}}
```

**Expected Error Response**:
```json
{
    "jsonrpc": "2.0",
    "id": 1,
    "error": {
        "code": -32043,
        "message": "Missing required prompt arguments",
        "data": {
            "missing_arguments": ["language"],
            "provided_arguments": []
        }
    }
}
```

**Test Verification**:
```erlang
test_error_code_32043() ->
    ProvidedArgs = #{},
    Arguments = [#mcp_prompt_argument{name = <<"language">>, required = true}],
    {error, {Code, Message, Data}} =
        erlmcp_prompt_argument_validator:validate_prompt_arguments(
            ProvidedArgs, Arguments, undefined
        ),
    ?assertEqual(?MCP_ERROR_PROMPT_ARGUMENT_MISSING, Code),  % -32043
    ?assertEqual(<<"Missing required prompt arguments">>, Message),
    ?assertEqual([<<"language">>], maps:get(<<"missing_arguments">>, Data)).
```

### Error Code -32045: Invalid Prompt Arguments

**When It Should Occur**:
- Argument provided but fails JSON Schema validation
- Type mismatch, enum violation, pattern mismatch, etc.

**Current Code** (validator, line 120):
```erlang
{error, {?JSONRPC_INVALID_PARAMS,  % ← Should be ?MCP_ERROR_INVALID_PROMPT_ARGUMENTS
    <<"Argument validation failed">>, #{
        <<"validation_errors">> => FormattedErrors
    }}}
```

**Fix Required**:
```erlang
{error, {?MCP_ERROR_INVALID_PROMPT_ARGUMENTS,  % -32045
    <<"Argument validation failed">>, #{
        <<"validation_errors">> => FormattedErrors
    }}}
```

**Expected Error Response**:
```json
{
    "jsonrpc": "2.0",
    "id": 2,
    "error": {
        "code": -32045,
        "message": "Argument validation failed",
        "data": {
            "validation_errors": [
                {
                    "path": "$.language",
                    "error": "Wrong type, expected: string"
                }
            ]
        }
    }
}
```

**Test Verification**:
```erlang
test_error_code_32045() ->
    ProvidedArgs = #{<<"language">> => 123},
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{<<"language">> => #{<<"type">> => <<"string">>}}
    },
    {error, {Code, Message, Data}} =
        erlmcp_prompt_argument_validator:validate_prompt_arguments(
            ProvidedArgs, undefined, Schema
        ),
    ?assertEqual(?MCP_ERROR_INVALID_PROMPT_ARGUMENTS, Code),  % -32045
    ?assertEqual(<<"Argument validation failed">>, Message),
    ValidationErrors = maps:get(<<"validation_errors">>, Data),
    ?assert(length(ValidationErrors) > 0).
```

### Data Field Requirements

**MCP Spec** requires error `data` field to contain:
- **For -32043**: Which arguments are missing, which were provided
- **For -32045**: Specific validation failures with JSON paths

**Current Implementation**: ✅ **CORRECT** - Data fields properly populated

**Verification**:
```erlang
test_error_data_field_32043() ->
    ProvidedArgs = #{<<"foo">> => <<"bar">>},
    Arguments = [
        #mcp_prompt_argument{name = <<"language">>, required = true},
        #mcp_prompt_argument{name = <<"style">>, required = true}
    ],
    {error, {_, _, Data}} =
        erlmcp_prompt_argument_validator:validate_prompt_arguments(
            ProvidedArgs, Arguments, undefined
        ),
    ?assertEqual([<<"language">>, <<"style">>], lists:sort(maps:get(<<"missing_arguments">>, Data))),
    ?assertEqual([<<"foo">>], maps:get(<<"provided_arguments">>, Data)).

test_error_data_field_32045() ->
    ProvidedArgs = #{<<"language">> => <<"java">>},
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"language">> => #{
                <<"type">> => <<"string">>,
                <<"enum">> => [<<"erlang">>, <<"elixir">>]
            }
        }
    },
    {error, {_, _, Data}} =
        erlmcp_prompt_argument_validator:validate_prompt_arguments(
            ProvidedArgs, undefined, Schema
        ),
    ValidationErrors = maps:get(<<"validation_errors">>, Data),
    ?assert(is_list(ValidationErrors)),
    [FirstError | _] = ValidationErrors,
    ?assert(maps:is_key(<<"path">>, FirstError)),
    ?assert(maps:is_key(<<"error">>, FirstError)).
```

---

## 11. Step 9: End-to-End Workflow Test

### Scenario: Code Review Prompt with Arguments

**Objective**: Test complete workflow from client request to handler execution with argument validation.

### Step 1: Start Server
```erlang
{ok, ServerPid} = erlmcp_server:start_link([{transport, stdio}]).
```

### Step 2: Register Prompt with Arguments and Schema
```erlang
erlmcp_server:add_prompt_with_args_and_schema(
    ServerPid,
    <<"code_review">>,
    <<"Review code for quality and best practices">>,
    [
        #mcp_prompt_argument{
            name = <<"language">>,
            description = <<"Programming language (erlang, elixir, python)">>,
            required = true
        },
        #mcp_prompt_argument{
            name = <<"style">>,
            description = <<"Code style guide to apply">>,
            required = false
        },
        #mcp_prompt_argument{
            name = <<"severity">>,
            description = <<"Minimum severity level (info, warning, error)">>,
            required = false
        }
    ],
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"language">> => #{
                <<"type">> => <<"string">>,
                <<"enum">> => [<<"erlang">>, <<"elixir">>, <<"python">>]
            },
            <<"style">> => #{
                <<"type">> => <<"string">>,
                <<"pattern">> => "^[a-z_]+$"
            },
            <<"severity">> => #{
                <<"type">> => <<"string">>,
                <<"enum">> => [<<"info">>, <<"warning">>, <<"error">>]
            }
        },
        <<"required">> => [<<"language">>],
        <<"additionalProperties">> => false
    },
    fun(Arguments) ->
        Language = maps:get(<<"language">>, Arguments),
        Style = maps:get(<<"style">>, Arguments, <<"default">>),
        Severity = maps:get(<<"severity">>, Arguments, <<"warning">>),

        Prompt = iolist_to_binary(io_lib:format(
            "Review my ~s code following ~s style guide. Report issues with severity >= ~s.",
            [Language, Style, Severity]
        )),

        [#mcp_prompt_message{
            role = <<"user">>,
            content = #mcp_text_content{text = Prompt}
        }]
    end
).
```

### Test 1: Valid Arguments → Success

**Request**:
```json
{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "prompts/get",
    "params": {
        "name": "code_review",
        "arguments": {
            "language": "erlang",
            "style": "otp_style",
            "severity": "warning"
        }
    }
}
```

**Expected Response**:
```json
{
    "jsonrpc": "2.0",
    "id": 1,
    "result": {
        "description": "Review code for quality and best practices",
        "messages": [
            {
                "role": "user",
                "content": {
                    "type": "text",
                    "text": "Review my erlang code following otp_style style guide. Report issues with severity >= warning."
                }
            }
        ]
    }
}
```

**Verification**:
```erlang
test_e2e_valid_arguments() ->
    {ok, ServerPid} = erlmcp_server:start_link([{transport, stdio}]),
    register_code_review_prompt(ServerPid),

    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"prompts/get">>,
        <<"params">> => #{
            <<"name">> => <<"code_review">>,
            <<"arguments">> => #{
                <<"language">> => <<"erlang">>,
                <<"style">> => <<"otp_style">>,
                <<"severity">> => <<"warning">>
            }
        }
    },

    %% Send request and capture response
    Response = send_request(ServerPid, Request),

    ?assertMatch(#{<<"jsonrpc">> := <<"2.0">>, <<"id">> := 1, <<"result">> := _}, Response),
    Result = maps:get(<<"result">>, Response),
    ?assertMatch(#{<<"messages">> := [_|_]}, Result),

    erlmcp_server:stop(ServerPid).
```

### Test 2: Missing Required Argument → Error -32043

**Request**:
```json
{
    "jsonrpc": "2.0",
    "id": 2,
    "method": "prompts/get",
    "params": {
        "name": "code_review",
        "arguments": {
            "style": "otp_style"
        }
    }
}
```

**Expected Response**:
```json
{
    "jsonrpc": "2.0",
    "id": 2,
    "error": {
        "code": -32043,
        "message": "Missing required prompt arguments",
        "data": {
            "missing_arguments": ["language"],
            "provided_arguments": ["style"]
        }
    }
}
```

**Verification**:
```erlang
test_e2e_missing_required_argument() ->
    {ok, ServerPid} = erlmcp_server:start_link([{transport, stdio}]),
    register_code_review_prompt(ServerPid),

    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 2,
        <<"method">> => <<"prompts/get">>,
        <<"params">> => #{
            <<"name">> => <<"code_review">>,
            <<"arguments">> => #{<<"style">> => <<"otp_style">>}
        }
    },

    Response = send_request(ServerPid, Request),

    ?assertMatch(#{<<"jsonrpc">> := <<"2.0">>, <<"id">> := 2, <<"error">> := _}, Response),
    Error = maps:get(<<"error">>, Response),
    ?assertEqual(?MCP_ERROR_PROMPT_ARGUMENT_MISSING, maps:get(<<"code">>, Error)),

    Data = maps:get(<<"data">>, Error),
    ?assertEqual([<<"language">>], maps:get(<<"missing_arguments">>, Data)),

    erlmcp_server:stop(ServerPid).
```

### Test 3: Invalid Argument Type → Error -32045

**Request**:
```json
{
    "jsonrpc": "2.0",
    "id": 3,
    "method": "prompts/get",
    "params": {
        "name": "code_review",
        "arguments": {
            "language": "java",
            "style": "CamelCase"
        }
    }
}
```

**Expected Response**:
```json
{
    "jsonrpc": "2.0",
    "id": 3,
    "error": {
        "code": -32045,
        "message": "Argument validation failed",
        "data": {
            "validation_errors": [
                {
                    "path": "$.language",
                    "error": "Value not in enum: [\"erlang\",\"elixir\",\"python\"]"
                },
                {
                    "path": "$.style",
                    "error": "Pattern does not match"
                }
            ]
        }
    }
}
```

**Verification**:
```erlang
test_e2e_invalid_argument_type() ->
    {ok, ServerPid} = erlmcp_server:start_link([{transport, stdio}]),
    register_code_review_prompt(ServerPid),

    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 3,
        <<"method">> => <<"prompts/get">>,
        <<"params">> => #{
            <<"name">> => <<"code_review">>,
            <<"arguments">> => #{
                <<"language">> => <<"java">>,
                <<"style">> => <<"CamelCase">>
            }
        }
    },

    Response = send_request(ServerPid, Request),

    ?assertMatch(#{<<"jsonrpc">> := <<"2.0">>, <<"id">> := 3, <<"error">> := _}, Response),
    Error = maps:get(<<"error">>, Response),
    ?assertEqual(?MCP_ERROR_INVALID_PROMPT_ARGUMENTS, maps:get(<<"code">>, Error)),

    Data = maps:get(<<"data">>, Error),
    ValidationErrors = maps:get(<<"validation_errors">>, Data),
    ?assert(length(ValidationErrors) >= 2),  % At least 2 errors (enum + pattern)

    erlmcp_server:stop(ServerPid).
```

### Helper Functions
```erlang
register_code_review_prompt(ServerPid) ->
    erlmcp_server:add_prompt_with_args_and_schema(
        ServerPid,
        <<"code_review">>,
        <<"Review code for quality and best practices">>,
        [
            #mcp_prompt_argument{name = <<"language">>, required = true},
            #mcp_prompt_argument{name = <<"style">>, required = false},
            #mcp_prompt_argument{name = <<"severity">>, required = false}
        ],
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"language">> => #{
                    <<"type">> => <<"string">>,
                    <<"enum">> => [<<"erlang">>, <<"elixir">>, <<"python">>]
                },
                <<"style">> => #{
                    <<"type">> => <<"string">>,
                    <<"pattern">> => "^[a-z_]+$"
                },
                <<"severity">> => #{
                    <<"type">> => <<"string">>,
                    <<"enum">> => [<<"info">>, <<"warning">>, <<"error">>]
                }
            },
            <<"required">> => [<<"language">>],
            <<"additionalProperties">> => false
        },
        fun(Args) ->
            Language = maps:get(<<"language">>, Args),
            Style = maps:get(<<"style">>, Args, <<"default">>),
            Severity = maps:get(<<"severity">>, Args, <<"warning">>),
            Prompt = iolist_to_binary(io_lib:format(
                "Review my ~s code following ~s style guide. Report issues with severity >= ~s.",
                [Language, Style, Severity]
            )),
            [#mcp_prompt_message{
                role = <<"user">>,
                content = #mcp_text_content{text = Prompt}
            }]
        end
    ).

send_request(ServerPid, Request) ->
    %% Implementation depends on test client setup
    %% Use erlmcp_test_client or direct gen_server calls
    erlmcp_test_client:send_sync(ServerPid, Request).
```

---

## 12. Verification Checklist

**Pre-Deployment Checklist**:

### Phase 1: Quick Fix (30 minutes)
- [ ] **File Renamed**: `.erl.broken` → `.erl`
- [ ] **Clean Rebuild**: `TERM=dumb rebar3 clean && TERM=dumb rebar3 compile`
- [ ] **Compilation Successful**: 0 errors, 0 warnings
- [ ] **BEAM File Exists**: `/home/user/erlmcp/_build/default/lib/erlmcp_core/ebin/erlmcp_prompt_argument_validator.beam`
- [ ] **Module Loads**: `code:which(erlmcp_prompt_argument_validator)` returns path
- [ ] **Validator Called**: Line 2019 in `erlmcp_server.erl` active
- [ ] **Basic Test Passes**: `rebar3 eunit --module=erlmcp_prompt_argument_validation_tests`

### Phase 2: Full Integration (2-3 hours)
- [ ] **Error Code -32043 Generated**: Missing argument test passes
- [ ] **Error Code -32045 Generated**: Invalid argument test passes
- [ ] **Error Responses Have Data**: Validation details in `data` field
- [ ] **JSON Schema Validation Works**: Type mismatches detected
- [ ] **Required Argument Validation Works**: Missing args detected
- [ ] **End-to-End Test 1 Passes**: Valid arguments → successful execution
- [ ] **End-to-End Test 2 Passes**: Missing argument → error -32043
- [ ] **End-to-End Test 3 Passes**: Invalid argument → error -32045
- [ ] **Coverage ≥80%**: All validator functions covered
- [ ] **Integration Tests Pass**: `rebar3 ct --suite=erlmcp_spec_compliance_SUITE`

### Phase 3: Quality Gates
- [ ] **All Unit Tests Pass**: 100% pass rate
- [ ] **Dialyzer Clean**: 0 type warnings
- [ ] **Xref Clean**: 0 undefined functions
- [ ] **No Regressions**: Existing tests still pass

### Post-Deployment Verification
- [ ] **Production Smoke Test**: Send real prompts/get request
- [ ] **Error Logs Clean**: No unexpected errors in logs
- [ ] **Performance Check**: No latency regression (<10ms added)

---

## 13. Rollback Procedure

**If Issues Arise After Renaming**:

### Rollback Step 1: Disable Validator
```bash
cd /home/user/erlmcp/apps/erlmcp_core/src/

# Rename back to .broken
mv erlmcp_prompt_argument_validator.erl erlmcp_prompt_argument_validator.erl.broken

# Verify
ls -la erlmcp_prompt_argument_validator.erl.broken
```

### Rollback Step 2: Clean Rebuild
```bash
cd /home/user/erlmcp

# Remove all build artifacts
rm -rf _build

# Rebuild without validator
TERM=dumb rebar3 compile
```

### Rollback Step 3: Verify Server Still Works
```bash
# Run server tests
rebar3 eunit --module=erlmcp_server_tests

# Expected: Tests pass (validation skipped)
```

### Rollback Step 4: Document Issue
Create issue in `/home/user/erlmcp/docs/ISSUES/validator_rollback_<date>.md`:
```markdown
# Validator Rollback - <Date>

## Reason
[Describe why rollback was necessary]

## Issues Encountered
- Compilation errors: [details]
- Runtime errors: [details]
- Test failures: [details]

## Next Steps
- Fix underlying issues
- Re-test in isolated environment
- Re-enable when ready
```

### Rollback Verification
- [ ] `.erl.broken` file exists
- [ ] No `.erl` file present
- [ ] Clean rebuild successful
- [ ] Server starts without errors
- [ ] Existing functionality works (validation skipped)

---

## 14. Timeline

### Quick Fix Timeline (30 minutes)

| Task | Duration | Cumulative |
|------|----------|------------|
| Rename file | 1 min | 1 min |
| Clean build | 2 min | 3 min |
| Verify compilation | 2 min | 5 min |
| Review module | 5 min | 10 min |
| Create basic tests | 10 min | 20 min |
| Run tests | 5 min | 25 min |
| Verification | 5 min | 30 min |
| **Total** | **30 min** | |

### Full Integration Timeline (2-3 hours)

| Task | Duration | Cumulative |
|------|----------|------------|
| **Phase 1d Quick Fix** | 30 min | 30 min |
| Update error codes (-32043, -32045) | 10 min | 40 min |
| Create comprehensive test suite | 60 min | 1h 40min |
| End-to-end workflow tests | 30 min | 2h 10min |
| Integration verification | 20 min | 2h 30min |
| Documentation updates | 30 min | 3h 00min |
| **Total** | **3h 00min** | |

### Milestone Breakdown

**Milestone 1: Validator Enabled** (30 min)
- File renamed
- Compiles successfully
- Basic tests pass
- **Deliverable**: Working validator (no error code fixes yet)

**Milestone 2: Error Codes Fixed** (40 min)
- Error codes updated to -32043, -32045
- Comprehensive test coverage
- **Deliverable**: MCP-compliant validation

**Milestone 3: Full Integration** (2h 30min)
- End-to-end tests pass
- Integration tests pass
- Documentation complete
- **Deliverable**: Production-ready validation

---

## 15. Full Integration Timeline (With Testing)

### Phase 1d: Validator Recovery (30 minutes)
**Steps 1-9 from this document**

**Deliverables**:
- Validator module renamed and compiled
- Basic validation tests passing
- Error responses generated (generic codes)

**Blockers**: None

### Phase 2a: Error Code Updates (10 minutes)

**Tasks**:
1. Update `validate_json_schema/2` error code (line 120):
   - From: `?JSONRPC_INVALID_PARAMS` (-32602)
   - To: `?MCP_ERROR_INVALID_PROMPT_ARGUMENTS` (-32045)

2. Update `validate_required_arguments/2` error code (line 156):
   - From: `?JSONRPC_INVALID_PARAMS` (-32602)
   - To: `?MCP_ERROR_PROMPT_ARGUMENT_MISSING` (-32043)

3. Verify macros defined in `erlmcp.hrl`:
   ```erlang
   -define(MCP_ERROR_PROMPT_ARGUMENT_MISSING, -32043).
   -define(MCP_ERROR_INVALID_PROMPT_ARGUMENTS, -32045).
   ```

**Deliverables**:
- MCP-compliant error codes
- Tests updated to expect correct codes

**Blockers**: Macros must be defined in `erlmcp.hrl`

### Phase 2b: Comprehensive Test Suite (1-2 hours)

**Test Categories**:
1. **Unit Tests** (30 min):
   - JSON Schema validation (10 tests)
   - Required argument validation (5 tests)
   - Error formatting (4 tests)
   - Edge cases (5 tests)

2. **Integration Tests** (30 min):
   - Server integration (4 tests)
   - Error response formatting (3 tests)
   - Transport-level validation (3 tests)

3. **End-to-End Tests** (30 min):
   - Valid arguments workflow (1 test)
   - Missing arguments workflow (1 test)
   - Invalid arguments workflow (1 test)
   - Complex schema validation (2 tests)

**Deliverables**:
- 40+ tests covering all validator paths
- ≥80% code coverage
- All tests passing

**Blockers**: None

### Phase 2c: Documentation (30 minutes)

**Documents to Update**:
1. `/home/user/erlmcp/docs/GAP_ANALYSIS.md`:
   - Mark Gap #42 as **COMPLETE**
   - Add implementation notes

2. `/home/user/erlmcp/docs/API_REFERENCE.md`:
   - Document `erlmcp_prompt_argument_validator` module
   - Add error code reference

3. `/home/user/erlmcp/docs/ERROR_CODES.md`:
   - Add -32043 and -32045 examples
   - Document validation error format

4. `/home/user/erlmcp/CHANGELOG.md`:
   - Add entry for validator recovery

**Deliverables**:
- Updated documentation
- Error code reference guide

**Blockers**: None

### Total Timeline Summary

| Phase | Task | Duration | Dependencies |
|-------|------|----------|--------------|
| 1d | Rename & Compile | 10 min | None |
| 1d | Basic Tests | 20 min | Compilation |
| 2a | Update Error Codes | 10 min | Phase 1d |
| 2b | Comprehensive Tests | 1-2 hours | Phase 2a |
| 2c | Documentation | 30 min | Phase 2b |
| **Total** | | **2-3 hours** | |

### Critical Path
```
Rename File (1 min)
    ↓
Compile (2 min)
    ↓
Basic Tests (20 min)
    ↓
Update Error Codes (10 min)
    ↓
Comprehensive Tests (1-2 hours)
    ↓
Documentation (30 min)
    ↓
DONE (2-3 hours total)
```

### Success Criteria
- [ ] Validator compiles with 0 errors
- [ ] All 40+ tests pass (100% pass rate)
- [ ] Coverage ≥80%
- [ ] Error codes -32043, -32045 returned correctly
- [ ] Documentation updated
- [ ] No regressions in existing tests

---

## Appendix A: Error Code Definitions

### MCP Error Codes (Spec 2025-11-25)

| Code | Name | Description | When to Use |
|------|------|-------------|-------------|
| -32043 | PromptArgumentMissing | Required argument not provided | Required argument missing in `arguments` map |
| -32045 | InvalidPromptArguments | Argument fails validation | Type mismatch, enum violation, pattern failure |

### JSON-RPC Error Codes

| Code | Name | Description | When to Use |
|------|------|-------------|-------------|
| -32602 | InvalidParams | Invalid method parameters | Generic parameter validation (fallback) |
| -32603 | InternalError | Internal server error | Unexpected validator crash |

### Current vs. Target

| Scenario | Current Code | Target Code | Fix Required |
|----------|--------------|-------------|--------------|
| Missing required arg | -32602 | -32043 | ✅ Yes (line 156) |
| Type mismatch | -32602 | -32045 | ✅ Yes (line 120) |
| Schema parse error | -32602 | -32602 | ❌ No (correct) |

---

## Appendix B: Dependencies

### Required Dependencies

**jesse** (JSON Schema Validator):
- **Version**: 1.8.0 or compatible
- **Purpose**: Validate arguments against JSON Schema
- **Usage**: Line 115 - `jesse:validate_with_schema/3`

**Verification**:
```bash
grep jesse /home/user/erlmcp/rebar.config

# Expected:
# {jesse, "1.8.0"}
```

**If Missing**:
```erlang
%% Add to rebar.config deps:
{deps, [
    {jesse, "1.8.0"}
]}.
```

---

## Appendix C: Testing Strategy

### Chicago School TDD Principles

**Applied to Validator**:
1. **Test observable behavior only** - No state inspection, no private function testing
2. **Test through public API** - `validate_prompt_arguments/3` only
3. **No mocks** - Use real jesse library, real records
4. **Integration over isolation** - Test validator + server integration

### Coverage Strategy

**Minimum 80% coverage** of:
- `validate_prompt_arguments/3` - Public API
- `do_validate/3` - Orchestration logic
- `validate_json_schema/2` - Schema validation
- `validate_required_arguments/2` - Required arg checking

**Acceptable <80% coverage** for:
- `format_jesse_errors/1` - Error formatting (not critical path)
- `format_error_message/1` - Message formatting (cosmetic)

---

## Appendix D: Known Issues

### Issue 1: gen_server Overhead
**Description**: Validator uses gen_server pattern, adding ~100μs overhead per validation.

**Impact**: Low - Validation is not on hot path, prompts/get is user-initiated.

**Mitigation**: None required. gen_server provides process isolation and timeout handling.

### Issue 2: jesse Dependency
**Description**: jesse library required for JSON Schema validation.

**Impact**: Medium - If jesse unavailable, schema validation will fail.

**Mitigation**: Ensure jesse in deps, fallback to required-only validation if jesse crashes.

### Issue 3: Error Code Mismatch
**Description**: Current implementation returns -32602 instead of -32043/-32045.

**Impact**: **HIGH** - Protocol compliance failure.

**Mitigation**: **REQUIRED FIX** in Phase 2a.

---

## Appendix E: Future Enhancements

### Enhancement 1: Async Validation
**Description**: Run validation in separate process to avoid blocking server.

**Benefit**: Improved server throughput under load.

**Effort**: Medium (2-3 hours).

### Enhancement 2: Validation Caching
**Description**: Cache schema compilation results to reduce overhead.

**Benefit**: Reduced latency for repeated validations.

**Effort**: Low (1 hour).

### Enhancement 3: Custom Validators
**Description**: Support custom validation functions beyond JSON Schema.

**Benefit**: Allow domain-specific validation logic (e.g., file path validation).

**Effort**: High (4-6 hours).

---

**End of Implementation Plan**

**Next Steps**: Execute Phase 1d (30 min quick fix) → Verify compilation → Run basic tests → Proceed to Phase 2 (error codes + comprehensive testing).
