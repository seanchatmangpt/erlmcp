# Phase 1c: Dialyzer & Type Safety Fixes - Implementation Plan

**Project**: erlmcp - Erlang/OTP MCP SDK
**Phase**: 1c - Type Safety & Dialyzer Compliance
**Status**: Ready for Implementation
**Estimated Time**: 12-15 hours (1.5-2 developer days)
**Priority**: High
**Date Created**: 2026-01-31

---

## Table of Contents

1. [Overview](#1-overview)
2. [Root Cause Analysis](#2-root-cause-analysis)
3. [Issue #1: Compilation Warnings (48 warnings)](#3-issue-1-compilation-warnings-48-warnings)
4. [Issue #2: Missing Type Specifications](#4-issue-2-missing-type-specifications)
5. [Issue #3: Unused Error Functions in erlmcp_json_rpc.erl](#5-issue-3-unused-error-functions-in-erlmcp_json_rpcةrl)
6. [Issue #4: Behaviour Conflicts](#6-issue-4-behaviour-conflicts)
7. [Issue #5: Debug Info Configuration](#7-issue-5-debug-info-configuration)
8. [Implementation Steps](#8-implementation-steps)
9. [Verification Checklist](#9-verification-checklist)
10. [Type Spec Examples](#10-type-spec-examples)
11. [Timeline](#11-timeline)
12. [Troubleshooting](#12-troubleshooting)

---

## 1. Overview

### Summary of Current State

Based on analysis from `docs/DIALYZER_REPORT.md` and `docs/DIALYZER_DEBUG_INFO_ANALYSIS.md`:

**Current Issues:**
- **48 compilation warnings**: 15 high-priority, 25 medium-priority, 8 low-priority
- **Missing type specs**: Core modules (erlmcp_server.erl, erlmcp_client.erl, erlmcp_json_rpc.erl) have 0-20% spec coverage
- **Unused error functions**: ~40+ error helper functions in erlmcp_json_rpc.erl may be unused
- **Behaviour conflicts**: erlmcp_transport_http.erl has conflicting behaviours
- **Debug info present**: rebar.config ALREADY has `debug_info` flag (line 13), but some beam files may lack it

**Impact on Production:**
- Type safety compromised - potential runtime errors not caught at compile time
- Dialyzer cannot perform full type analysis on modules without debug_info
- Code maintainability reduced - missing specs make API unclear
- Refactoring risk increased - no compile-time verification of type contracts

**Good News:**
- TCPS modules have excellent type coverage (tcps_kanban: 100%, tcps_andon: 100%, tcps_work_order: 100%)
- rebar.config already has debug_info enabled (line 13)
- Infrastructure in place for type checking

---

## 2. Root Cause Analysis

### 2.1 Why Are There Compilation Warnings?

**Root Causes Identified:**

1. **Ambiguous BIF Calls** (11 occurrences)
   - Modules call `binary_to_integer/1`, `atom_to_binary/1`, `error/2` without module prefix
   - Dialyzer cannot determine if these are stdlib BIFs or local overrides
   - **Files affected:**
     - `apps/erlmcp_core/src/erlmcp_version.erl` (7 occurrences)
     - `apps/tcps_erlmcp/src/tcps_rebar3_receipt.erl` (2 occurrences)
     - `apps/tcps_erlmcp/src/tcps_cli_receipt.erl` (1 occurrence)
     - `apps/tcps_erlmcp/src/tcps_cli_format.erl` (1 occurrence)

2. **Unused Variables** (8 occurrences)
   - Pattern matching extracts variables that are never used
   - Indicates incomplete implementation or dead code
   - **Files affected:**
     - `apps/tcps_erlmcp/src/tcps_cli_kaizen.erl` (line 45)
     - `apps/tcps_erlmcp/src/tcps_kaizen.erl` (lines 132, 840, 1057, 1209, 1214)
     - `apps/tcps_erlmcp/src/tcps_persistence.erl` (line 820 - two variables)

3. **OTP 27 Float Matching** (4 occurrences)
   - Pattern matching on `0.0` won't match `-0.0` in OTP 27+
   - Future compatibility issue
   - **Files affected:**
     - `apps/tcps_erlmcp/src/tcps_cli_quality.erl` (line 281)
     - `apps/tcps_erlmcp/src/tcps_kaizen.erl` (lines 936, 1137, 1144)

4. **Behaviour Conflicts** (1 CRITICAL)
   - `erlmcp_transport_http.erl` implements both `gen_server` and `erlmcp_transport` behaviours
   - Both define `init/1` callback with different signatures
   - **File affected:**
     - `apps/erlmcp_transports/src/erlmcp_transport_http.erl` (line 3)

5. **Unused Functions** (3 occurrences)
   - Functions defined but never called
   - Potential dead code or planned features
   - **Files affected:**
     - `apps/tcps_erlmcp/src/tcps_receipt_verifier.erl` (line 746: `is_atom_stage/1`)
     - `apps/tcps_erlmcp/src/tcps_work_order.erl` (line 2033: `atom_to_binary/1`)
     - `apps/erlmcp_core/src/erlmcp.erl` (line 601: `default_pool_config/1`)

### 2.2 Why Are Type Specs Missing?

**Root Causes:**

1. **Gradual development approach**: Specs added as needed, not systematically
2. **nowarn_missing_spec flag**: rebar.config line 18 disables missing spec warnings
3. **No enforcement**: No pre-commit hook or CI gate for spec coverage
4. **Legacy code**: Some modules predate type spec discipline

**Current Spec Coverage:**
- ✅ TCPS modules: 95-100% (excellent)
- ⚠️ erlmcp_server.erl: 0% (CRITICAL)
- ⚠️ erlmcp_client.erl: 0% (CRITICAL)
- ⚠️ erlmcp_json_rpc.erl: 20% (HIGH)
- ⚠️ erlmcp_stdio.erl: 10% (HIGH)

### 2.3 Debug Info Status

**From rebar.config analysis:**

```erlang
% Line 12-25 in /home/user/erlmcp/rebar.config
{erl_opts, [
    debug_info,  % ✅ PRESENT - This is CORRECT
    nowarn_export_vars,
    nowarn_shadow_vars,
    nowarn_obsolete_guard,
    nowarn_unused_import,
    nowarn_missing_spec,
    nowarn_unused_function,
    nowarn_unused_type,
    nowarn_unused_vars,
    nowarn_unsafe_vars,
    {i, "include"},
    {platform_define, "^2[1-9]|^[3-9]", 'POST_OTP_21'}
]}.
```

**Status**: ✅ `debug_info` is ALREADY in rebar.config (line 13)

**However**, some beam files may still lack debug info due to:
1. Stale build artifacts from before debug_info was added
2. Production profile explicitly disabling it (was fixed, but old builds may exist)
3. Test modules compiled separately without inheriting erl_opts

**Solution**: Clean rebuild will regenerate all beam files with debug_info.

---

## 3. Issue #1: Compilation Warnings (48 warnings)

### 3.1 High Priority: Ambiguous BIF Calls (11 occurrences)

#### File: `apps/erlmcp_core/src/erlmcp_version.erl`

**Problem**: Lines 155-163, 231 call `binary_to_integer/1` without module prefix

**Current code (lines 155-163)**:
```erlang
Major = binary_to_integer(binary:list_to_bin(MajorStr)),
Minor = binary_to_integer(binary:list_to_bin(MinorStr)),
Patch = binary_to_integer(binary:list_to_bin(PatchStr)),
```

**Fix Option 1: Use fully qualified calls**
```erlang
Major = erlang:binary_to_integer(binary:list_to_bin(MajorStr)),
Minor = erlang:binary_to_integer(binary:list_to_bin(MinorStr)),
Patch = erlang:binary_to_integer(binary:list_to_bin(PatchStr)),
```

**Fix Option 2: Add compile directive (RECOMMENDED)**
```erlang
% Add at top of module (after -module declaration)
-compile({no_auto_import,[binary_to_integer/1]}).

% Then use qualified calls
Major = erlang:binary_to_integer(...),
```

**Lines to modify**: 155, 156, 157, 159, 160, 161, 163, 231

#### File: `apps/tcps_erlmcp/src/tcps_rebar3_receipt.erl`

**Problem**: Lines 238, 242 call `atom_to_binary/1` ambiguously

**Fix**:
```erlang
% Line 238 - Before:
StageAtom = atom_to_binary(Stage),

% Line 238 - After:
StageAtom = erlang:atom_to_binary(Stage, utf8),

% Line 242 - Before:
TypeAtom = atom_to_binary(Type),

% Line 242 - After:
TypeAtom = erlang:atom_to_binary(Type, utf8),
```

#### File: `apps/tcps_erlmcp/src/tcps_cli_receipt.erl`

**Problem**: Line 182 calls `atom_to_binary/1` ambiguously

**Fix**:
```erlang
% Line 182 - Before:
StageBin = atom_to_binary(Stage),

% Line 182 - After:
StageBin = erlang:atom_to_binary(Stage, utf8),
```

#### File: `apps/tcps_erlmcp/src/tcps_cli_format.erl`

**Problem**: Line 109 calls `error/2` ambiguously

**Fix**:
```erlang
% Line 109 - Before:
error(function_clause, [Term])

% Line 109 - After:
erlang:error(function_clause, [Term])
```

### 3.2 High Priority: Unused Variables (8 occurrences)

#### File: `apps/tcps_erlmcp/src/tcps_cli_kaizen.erl`

**Problem**: Line 45 - `StartDate` variable unused

**Fix**:
```erlang
% Line 45 - Before:
{ok, StartDate} = some_function(),

% Line 45 - After:
{ok, _StartDate} = some_function(),  % Prefix with underscore
```

#### File: `apps/tcps_erlmcp/src/tcps_kaizen.erl`

**Multiple unused variables:**

```erlang
% Line 132 - Before:
SkusGenerated = filter_receipts(Receipts, sku_generated),

% Line 132 - After:
_SkusGenerated = filter_receipts(Receipts, sku_generated),

% Line 840 - Before:
Improvement = lookup_improvement(Id),

% Line 840 - After:
_Improvement = lookup_improvement(Id),

% Line 1057 - Before:
Waste = calculate_waste(Process),

% Line 1057 - After:
_Waste = calculate_waste(Process),

% Lines 1209, 1214 - These are stub functions, add implementation or mark as intentional
% Option 1: Add -spec and return not_implemented
-spec lookup_improvement(ImprovementId :: binary()) ->
    {ok, improvement()} | {error, not_found | not_implemented}.
lookup_improvement(_ImprovementId) ->
    {error, not_implemented}.

-spec mark_improvement_applied(ImprovementId :: binary()) ->
    ok | {error, not_implemented}.
mark_improvement_applied(_ImprovementId) ->
    {error, not_implemented}.
```

#### File: `apps/tcps_erlmcp/src/tcps_persistence.erl`

**Problem**: Line 820 - `OntologyFiles` and `Query` unused (unimplemented function)

**Fix**:
```erlang
% Line 820 - Add -spec and mark as TODO
-spec execute_sparql_query(OntologyFiles :: [file:filename()],
                           Query :: binary()) ->
    {ok, Results :: [map()]} | {error, not_implemented}.
execute_sparql_query(_OntologyFiles, _Query) ->
    %% TODO: Implement SPARQL query execution
    {error, not_implemented}.
```

### 3.3 Medium Priority: OTP 27 Float Matching (4 occurrences)

#### File: `apps/tcps_erlmcp/src/tcps_cli_quality.erl`

**Problem**: Line 281 - Pattern match `0.0 -> 0.0` won't match `-0.0` in OTP 27+

**Fix**:
```erlang
% Line 281 - Before:
case Coverage of
    0.0 -> 0.0;
    C when C > 0 -> C
end.

% Line 281 - After (OTP 27 compatible):
case Coverage of
    +0.0 -> +0.0;  % Explicitly match positive zero
    C when C > 0 -> C
end.
```

#### File: `apps/tcps_erlmcp/src/tcps_kaizen.erl`

**Multiple float matching issues:**

```erlang
% Line 936 - Before:
{_, 0.0} -> 0.0;

% Line 936 - After:
{_, +0.0} -> +0.0;

% Line 1137 - Before:
Target =:= 0.0 -> 0.0;

% Line 1137 - After:
Target =:= +0.0 -> +0.0;

% Line 1144 - Before:
case Result of
    0.0 -> default_value;
    _ -> Result
end.

% Line 1144 - After:
case Result of
    +0.0 -> default_value;
    _ -> Result
end.
```

### 3.4 Low Priority: Unused Functions (3 occurrences)

#### File: `apps/tcps_erlmcp/src/tcps_receipt_verifier.erl`

**Problem**: Line 746 - `is_atom_stage/1` never called

**Fix Options**:

**Option 1: Remove function** (if truly dead code)
```erlang
% Delete lines around 746:
% is_atom_stage(Stage) when is_atom(Stage) -> true;
% is_atom_stage(_) -> false.
```

**Option 2: Add nowarn pragma** (if planning to use later)
```erlang
% Add at top of module:
-compile({nowarn_unused_function, [{is_atom_stage, 1}]}).
```

**Option 3: Export function** (if part of public API)
```erlang
% Add to -export list:
-export([..., is_atom_stage/1]).
```

#### File: `apps/tcps_erlmcp/src/tcps_work_order.erl`

**Problem**: Line 2033 - `atom_to_binary/1` helper never called

**Fix**: Same options as above. Likely Option 1 (remove) or Option 2 (nowarn).

#### File: `apps/erlmcp_core/src/erlmcp.erl`

**Problem**: Line 601 - `default_pool_config/1` unused

**Analysis**: This might be a planned API function for connection pooling.

**Recommended Fix**: Export function if it's part of public API, or add nowarn pragma if it's for future use.

---

## 4. Issue #2: Missing Type Specifications

### 4.1 Critical: erlmcp_server.erl (0% coverage)

**Location**: `apps/erlmcp_core/src/erlmcp_server.erl`

**Current Exports** (estimated from MCP server pattern):
```erlang
-export([
    start_link/1,
    stop/1,
    add_tool/3,
    add_resource/3,
    add_prompt/3,
    remove_tool/2,
    remove_resource/2,
    remove_prompt/2,
    call_tool/3,
    read_resource/2,
    get_prompt/2,
    subscribe_resource/2,
    unsubscribe_resource/2
]).
```

**Missing Specs** (need to add ~15 specs):

```erlang
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

-spec stop(pid()) -> ok.
stop(ServerPid) ->
    gen_server:stop(ServerPid).

-spec add_tool(pid(), binary(), fun()) -> ok | {error, term()}.
add_tool(ServerPid, ToolName, HandlerFun) ->
    gen_server:call(ServerPid, {add_tool, ToolName, HandlerFun}).

-spec add_resource(pid(), binary(), fun()) -> ok | {error, term()}.
add_resource(ServerPid, Uri, HandlerFun) ->
    gen_server:call(ServerPid, {add_resource, Uri, HandlerFun}).

-spec add_prompt(pid(), binary(), fun()) -> ok | {error, term()}.
add_prompt(ServerPid, PromptName, HandlerFun) ->
    gen_server:call(ServerPid, {add_prompt, PromptName, HandlerFun}).

-spec call_tool(pid(), binary(), map()) ->
    {ok, map()} | {error, term()}.
call_tool(ServerPid, ToolName, Args) ->
    gen_server:call(ServerPid, {call_tool, ToolName, Args}, 30000).

-spec read_resource(pid(), binary()) ->
    {ok, map()} | {error, term()}.
read_resource(ServerPid, Uri) ->
    gen_server:call(ServerPid, {read_resource, Uri}).

-spec subscribe_resource(pid(), binary()) -> ok | {error, term()}.
subscribe_resource(ServerPid, Uri) ->
    gen_server:call(ServerPid, {subscribe_resource, Uri}).
```

**Callback Specs**:
```erlang
-spec init([map()]) -> {ok, state()}.
-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()}.
-spec handle_cast(term(), state()) -> {noreply, state()}.
-spec handle_info(term(), state()) -> {noreply, state()}.
-spec terminate(term(), state()) -> ok.
-spec code_change(term(), state(), term()) -> {ok, state()}.
```

### 4.2 Critical: erlmcp_client.erl (0% coverage)

**Location**: `apps/erlmcp_core/src/erlmcp_client.erl`

**Estimated Exports**:
```erlang
-export([
    start_link/1,
    stop/1,
    initialize/2,
    list_tools/1,
    list_resources/1,
    list_prompts/1,
    call_tool/3,
    read_resource/2,
    get_prompt/2,
    complete/3
]).
```

**Missing Specs** (~12 specs needed):

```erlang
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

-spec stop(pid()) -> ok.
stop(ClientPid) ->
    gen_server:stop(ClientPid).

-spec initialize(pid(), map()) -> {ok, map()} | {error, term()}.
initialize(ClientPid, ClientInfo) ->
    gen_server:call(ClientPid, {initialize, ClientInfo}, 30000).

-spec list_tools(pid()) -> {ok, [map()]} | {error, term()}.
list_tools(ClientPid) ->
    gen_server:call(ClientPid, list_tools).

-spec list_resources(pid()) -> {ok, [map()]} | {error, term()}.
list_resources(ClientPid) ->
    gen_server:call(ClientPid, list_resources).

-spec call_tool(pid(), binary(), map()) ->
    {ok, map()} | {error, term()}.
call_tool(ClientPid, ToolName, Arguments) ->
    gen_server:call(ClientPid, {call_tool, ToolName, Arguments}, 30000).

-spec read_resource(pid(), binary()) ->
    {ok, map()} | {error, term()}.
read_resource(ClientPid, Uri) ->
    gen_server:call(ClientPid, {read_resource, Uri}, 10000).
```

**Callback Specs**: Same pattern as server.

### 4.3 High Priority: erlmcp_json_rpc.erl (20% coverage)

**Location**: `apps/erlmcp_core/src/erlmcp_json_rpc.erl`

**Current Status**: Already has some specs (lines 70-91 have specs for core functions)

**Missing Specs**: Error helper functions (lines 480-877) mostly lack specs

**Strategy**:
1. Check which error functions are actually used (see Section 5)
2. Add specs only for used functions
3. Remove or document unused functions

**Example specs needed** (for used functions):

```erlang
% These already have specs:
-spec encode_request(json_rpc_id(), binary(), json_rpc_params()) -> binary().
-spec encode_response(json_rpc_id(), term()) -> binary().
-spec encode_error_response(json_rpc_id(), integer(), binary()) -> binary().

% Need to add for error helpers (if used):
-spec error_tool_description_too_large(json_rpc_id(), pos_integer(), pos_integer()) -> binary().
-spec error_invalid_content_type(json_rpc_id(), binary()) -> binary().
-spec error_content_too_large(json_rpc_id(), pos_integer(), pos_integer()) -> binary().
% ... etc for ~40 more functions
```

---

## 5. Issue #3: Unused Error Functions in erlmcp_json_rpc.erl

### 5.1 Analysis of Error Helper Functions

**File**: `apps/erlmcp_core/src/erlmcp_json_rpc.erl`

**Exported Error Functions** (lines 20-55):
- Total: 37 error helper functions exported
- Categories:
  - Content errors (lines 480-513): 4 functions
  - Resource errors (lines 520-558): 5 functions
  - Tool errors (lines 565-600): 4 functions
  - Prompt errors (lines 607-637): 3 functions
  - Auth errors (lines 650-678): 5 functions
  - Protocol errors (lines 685-714): 4 functions
  - Pagination errors (lines 721-748): 4 functions
  - Task errors (lines 755-789): 5 functions
  - Progress errors (lines 796-836): 5 functions
  - Completion errors (lines 843-876): 4 functions

**Purpose**: These functions are part of the MCP specification error code catalog (MCP 2025-11-25 spec compliance).

### 5.2 Determining Usage

**Strategy**:
1. Run xref analysis to find unused exports
2. Search codebase for references to each function
3. Categorize as:
   - **Used**: Keep with spec
   - **Unused but spec-required**: Keep with documentation comment
   - **Unused and non-essential**: Remove or add `-compile(nowarn_unused_export)`

**Command to check usage**:
```bash
# For each error function, check if it's used anywhere
grep -r "error_tool_description_too_large" apps/
grep -r "error_invalid_content_type" apps/
grep -r "error_content_too_large" apps/
# ... etc for all 37 functions
```

### 5.3 Decision Matrix

**Option 1: Keep All Functions (RECOMMENDED)**
- **Rationale**: These are MCP spec-compliant error codes
- **Action**: Add documentation explaining they're part of MCP protocol spec
- **Add to module header**:
```erlang
%% ====================================================================
%% MCP 2025-11-25 Specification Error Codes
%% ====================================================================
%% This module implements ALL error codes from the MCP specification,
%% even if not currently used. They are kept for:
%% 1. Protocol compliance
%% 2. Future feature support
%% 3. Client library consistency
%%
%% Reference: https://spec.modelcontextprotocol.io/specification/2025-11-25/errors/
%% ====================================================================
```

**Option 2: Remove Unused Functions**
- **Rationale**: Reduce code surface area, easier maintenance
- **Risk**: May need to add them back for full MCP compliance
- **Action**: Delete unused function definitions (lines 480-877 selectively)

**Option 3: Add Suppress Warnings (Middle Ground)**
- **Rationale**: Keep functions but silence xref warnings
- **Action**: Add at module level:
```erlang
-compile({nowarn_unused_function, [
    {error_tool_description_too_large, 3},
    {error_invalid_content_type, 2},
    {error_content_too_large, 3},
    % ... list all 37 functions
]}).
```

### 5.4 Recommended Action

**KEEP ALL ERROR FUNCTIONS** because:
1. They're part of MCP spec compliance
2. They may be used by client code we don't control
3. They're exported, so removing them is a breaking change
4. Documentation overhead is minimal

**Implementation**:
1. Add module-level documentation explaining why they exist
2. Add `-doc` attributes to each function:
```erlang
%% @doc MCP error -32011: Tool description exceeds size limit
-doc """
MCP specification error code -32011: Tool description too large.
Kept for protocol compliance with MCP 2025-11-25 specification.
""".
-spec error_tool_description_too_large(json_rpc_id(), pos_integer(), pos_integer()) -> binary().
error_tool_description_too_large(Id, ActualSize, MaxSize)
  when is_integer(ActualSize), is_integer(MaxSize) ->
    Data = #{
        <<"actualSize">> => ActualSize,
        <<"maxSize">> => MaxSize
    },
    encode_error_response(Id, ?MCP_ERROR_TOOL_DESCRIPTION_TOO_LONG,
                          ?MCP_MSG_TOOL_DESCRIPTION_TOO_LONG, Data).
```

---

## 6. Issue #4: Behaviour Conflicts

### 6.1 Critical: erlmcp_transport_http.erl

**File**: `apps/erlmcp_transports/src/erlmcp_transport_http.erl`

**Problem**: Lines 3-4 declare both behaviours:
```erlang
-behaviour(erlmcp_transport).
-behaviour(gen_server).
```

**Conflict**: Both behaviours require `init/1` callback:
- `gen_server:init/1` expects `init([term()]) -> {ok, State}`
- `erlmcp_transport:init/1` expects `init(Type, Opts) -> {ok, State}`

**Current Dialyzer Error**:
```
erlmcp_transport_http.erl:3: Conflicting callback definition for init/1
  Expected: init(list()) -> {ok, state()}     (from gen_server)
  Found:    init(atom(), map()) -> {ok, state()}  (from erlmcp_transport)
```

### 6.2 Solution Options

**Option 1: Remove erlmcp_transport Behaviour (RECOMMENDED)**

```erlang
% Line 3-4 - Before:
-behaviour(erlmcp_transport).
-behaviour(gen_server).

% Line 3-4 - After:
-behaviour(gen_server).

%% Manually implement erlmcp_transport callbacks without declaring behaviour
%% This avoids the conflict while still providing the transport interface
```

**Then implement transport API manually**:
```erlang
%% Transport API (manual implementation, not behaviour callback)
-export([transport_init/2, transport_send/2, transport_close/1]).

%% @doc Initialize transport (NOT a behaviour callback)
-spec transport_init(atom(), map()) -> {ok, pid()} | {error, term()}.
transport_init(Type, Opts) ->
    gen_server:start_link(?MODULE, {Type, Opts}, []).

%% @doc Send data via transport (NOT a behaviour callback)
-spec transport_send(pid(), binary()) -> ok | {error, term()}.
transport_send(Pid, Data) ->
    gen_server:call(Pid, {send, Data}).

%% @doc Close transport (NOT a behaviour callback)
-spec transport_close(pid()) -> ok.
transport_close(Pid) ->
    gen_server:stop(Pid).

%% gen_server callbacks (these ARE behaviour callbacks)
init({Type, Opts}) ->
    % Initialize gen_server state
    % Call transport_init_impl/2 internally
    State = #state{transport_type = Type, config = Opts},
    {ok, State}.
```

**Option 2: Use Delegation Pattern (More Complex)**

```erlang
-behaviour(gen_server).

%% Wrapper module that delegates to transport implementation
-record(state, {
    transport_pid :: pid(),
    transport_state :: term()
}).

init([{Type, Opts}]) ->
    % Start transport as separate process
    case erlmcp_transport_http_impl:init(Type, Opts) of
        {ok, TransportState} ->
            {ok, #state{transport_state = TransportState}};
        Error ->
            {stop, Error}
    end.
```

**Option 3: Restructure Module (Major Refactor)**

Split into two modules:
- `erlmcp_transport_http_server.erl` - gen_server implementation
- `erlmcp_transport_http_impl.erl` - transport behaviour implementation

**Recommended**: Option 1 (remove behaviour declaration, implement manually)

**Why**:
- Minimal code changes
- Maintains functionality
- Avoids Dialyzer conflict
- Clear separation of concerns

### 6.3 Implementation Steps for Option 1

1. **Remove behaviour declaration** (line 3)
2. **Add transport API exports** (not callbacks)
3. **Document why we're not using behaviour**:
```erlang
%% ====================================================================
%% Transport Implementation Note
%% ====================================================================
%% This module implements the erlmcp_transport interface manually
%% rather than using -behaviour(erlmcp_transport) to avoid callback
%% conflicts with gen_server behaviour.
%%
%% The transport API (transport_init/2, transport_send/2, transport_close/1)
%% is implemented as regular exported functions, not behaviour callbacks.
%% ====================================================================
```

---

## 7. Issue #5: Debug Info Configuration

### 7.1 Current rebar.config Analysis

**File**: `/home/user/erlmcp/rebar.config`

**Lines 12-25** (current erl_opts):
```erlang
{erl_opts, [
    debug_info,                  % ✅ PRESENT - Line 13
    nowarn_export_vars,
    nowarn_shadow_vars,
    nowarn_obsolete_guard,
    nowarn_unused_import,
    nowarn_missing_spec,         % ⚠️ Disables spec warnings (line 18)
    nowarn_unused_function,      % ⚠️ Disables unused function warnings (line 19)
    nowarn_unused_type,          % ⚠️ Disables unused type warnings (line 20)
    nowarn_unused_vars,          % ⚠️ Disables unused variable warnings (line 21)
    nowarn_unsafe_vars,          % ⚠️ Disables unsafe variable warnings (line 22)
    {i, "include"},
    {platform_define, "^2[1-9]|^[3-9]", 'POST_OTP_21'}
]}.
```

**Status**: ✅ debug_info is ALREADY present (line 13)

**Issue**: Too many `nowarn_*` flags suppress useful warnings

### 7.2 Production Profile Analysis

**Lines 77-88** (prod profile):
```erlang
{prod, [
    {erl_opts, [
        no_debug_info,           % ❌ BAD - Removes debug_info in production
        {d, 'PROD'}
    ]},
    {compiler_warnings_as_errors, true},
    {relx, [
        {dev_mode, false},
        {include_erts, true},
        {include_src, false}
    ]}
]}
```

**Problem**: `no_debug_info` in production profile (line 79)
- Removes debug_info from production builds
- Prevents crash analysis and debugging
- Not recommended for production (minimal size savings, major debugging cost)

### 7.3 Recommended Configuration Changes

**Option 1: Keep All Suppressions (Current State)**
- **Pro**: Allows gradual cleanup without blocking development
- **Con**: Hides real issues that should be fixed

**Option 2: Remove All Suppressions (Aggressive)**
```erlang
{erl_opts, [
    debug_info,
    % REMOVED: nowarn_export_vars,
    % REMOVED: nowarn_shadow_vars,
    % REMOVED: nowarn_obsolete_guard,
    % REMOVED: nowarn_unused_import,
    % REMOVED: nowarn_missing_spec,
    % REMOVED: nowarn_unused_function,
    % REMOVED: nowarn_unused_type,
    % REMOVED: nowarn_unused_vars,
    % REMOVED: nowarn_unsafe_vars,
    {i, "include"},
    {platform_define, "^2[1-9]|^[3-9]", 'POST_OTP_21'}
]}.
```
- **Pro**: Exposes all issues for fixing
- **Con**: May break compilation with 100+ warnings

**Option 3: Gradual Approach (RECOMMENDED)**

```erlang
{erl_opts, [
    debug_info,
    %% Phase 1: Keep these for now (remove after Phase 1c)
    nowarn_export_vars,
    nowarn_shadow_vars,
    nowarn_obsolete_guard,
    nowarn_unused_import,

    %% Phase 2: Remove after type specs added (target: Phase 2)
    nowarn_missing_spec,

    %% Phase 3: Remove after unused code cleanup (target: Phase 3)
    % nowarn_unused_function,  % TODO: Remove after cleanup
    % nowarn_unused_type,      % TODO: Remove after cleanup
    % nowarn_unused_vars,      % TODO: Remove after cleanup
    % nowarn_unsafe_vars,      % TODO: Remove after cleanup

    {i, "include"},
    {platform_define, "^2[1-9]|^[3-9]", 'POST_OTP_21'}
]}.
```

**For this Phase (1c)**: Keep all suppressions, focus on:
1. Fixing dialyzer-specific issues (BIF ambiguity, behaviour conflicts)
2. Adding missing type specs
3. Clean rebuild with debug_info

### 7.4 Production Profile Fix

**Change**:
```erlang
{prod, [
    {erl_opts, [
        debug_info,              % ✅ KEEP debug_info in production
        {d, 'PROD'}
    ]},
    {compiler_warnings_as_errors, true},
    {relx, [
        {dev_mode, false},
        {include_erts, true},
        {include_src, false}
    ]}
]}
```

**Rationale**:
- Debug info adds ~4-5% to binary size (acceptable for observability)
- Essential for crash dump analysis
- Enables runtime debugging without recompilation
- Industry best practice (Erlang/OTP itself ships with debug_info)

---

## 8. Implementation Steps

### Step 1: Clean Rebuild with Debug Info ⏱️ 5 minutes

**Purpose**: Ensure all beam files have debug_info

```bash
# 1.1: Remove all build artifacts
rm -rf _build

# 1.2: Compile with debug_info (already in rebar.config)
TERM=dumb rebar3 compile

# Expected output:
# ===> Verifying dependencies...
# ===> Analyzing applications...
# ===> Compiling erlmcp_core (92 modules)
# ===> Compiling erlmcp_transports (28 modules)
# ===> Compiling erlmcp_observability (21 modules)
# ===> Compiling erlmcp_validation (5 modules)

# 1.3: Verify debug_info presence in random sample
for module in erlmcp_auth erlmcp_session erlmcp_json_rpc; do
    echo "Checking ${module}.beam for debug_info..."
    strings _build/default/lib/erlmcp_core/ebin/${module}.beam | grep -q "Dbgi" && \
        echo "  ✅ ${module}: HAS debug_info" || \
        echo "  ❌ ${module}: MISSING debug_info"
done

# Expected output:
#   ✅ erlmcp_auth: HAS debug_info
#   ✅ erlmcp_session: HAS debug_info
#   ✅ erlmcp_json_rpc: HAS debug_info
```

**Success Criteria**:
- ✅ All modules compile without errors
- ✅ All beam files contain "Dbgi" chunk
- ✅ No "Could not get Core Erlang code" errors

---

### Step 2: Fix High-Priority Warnings ⏱️ 2-3 hours

#### 2.1: Fix Ambiguous BIF Calls (11 occurrences)

**File 1**: `apps/erlmcp_core/src/erlmcp_version.erl`

```bash
# Edit file
vim apps/erlmcp_core/src/erlmcp_version.erl
```

**Changes**:
```erlang
% Add after -module(erlmcp_version):
-compile({no_auto_import,[binary_to_integer/1]}).

% Then replace all occurrences (lines 155-163, 231):
% Before: Major = binary_to_integer(...)
% After:  Major = erlang:binary_to_integer(...)

% Use sed for bulk replacement:
sed -i 's/binary_to_integer(/erlang:binary_to_integer(/g' \
    apps/erlmcp_core/src/erlmcp_version.erl
```

**File 2**: `apps/tcps_erlmcp/src/tcps_rebar3_receipt.erl`

```erlang
% Lines 238, 242:
% Before: StageAtom = atom_to_binary(Stage),
% After:  StageAtom = erlang:atom_to_binary(Stage, utf8),

sed -i 's/atom_to_binary(\([^)]*\))/erlang:atom_to_binary(\1, utf8)/g' \
    apps/tcps_erlmcp/src/tcps_rebar3_receipt.erl
```

**File 3**: `apps/tcps_erlmcp/src/tcps_cli_receipt.erl`

```erlang
% Line 182:
sed -i 's/atom_to_binary(\([^)]*\))/erlang:atom_to_binary(\1, utf8)/g' \
    apps/tcps_erlmcp/src/tcps_cli_receipt.erl
```

**File 4**: `apps/tcps_erlmcp/src/tcps_cli_format.erl`

```erlang
% Line 109:
% Before: error(function_clause, [Term])
% After:  erlang:error(function_clause, [Term])

sed -i 's/\berror(function_clause,/erlang:error(function_clause,/g' \
    apps/tcps_erlmcp/src/tcps_cli_format.erl
```

**Verify**:
```bash
TERM=dumb rebar3 compile | grep -i "ambiguous\|warning"
# Expected: No ambiguous BIF warnings
```

#### 2.2: Fix Unused Variables (8 occurrences)

**File**: `apps/tcps_erlmcp/src/tcps_cli_kaizen.erl`
```erlang
% Line 45:
sed -i 's/{ok, StartDate}/{ok, _StartDate}/g' \
    apps/tcps_erlmcp/src/tcps_cli_kaizen.erl
```

**File**: `apps/tcps_erlmcp/src/tcps_kaizen.erl`
```erlang
% Multiple lines (132, 840, 1057):
# Manually edit to prefix with underscore:
vim apps/tcps_erlmcp/src/tcps_kaizen.erl

# Line 132:   SkusGenerated = ...    → _SkusGenerated = ...
# Line 840:   Improvement = ...      → _Improvement = ...
# Line 1057:  Waste = ...            → _Waste = ...

# Lines 1209, 1214: Add specs for stub functions:
-spec lookup_improvement(ImprovementId :: binary()) ->
    {ok, improvement()} | {error, not_found | not_implemented}.
lookup_improvement(_ImprovementId) ->
    {error, not_implemented}.

-spec mark_improvement_applied(ImprovementId :: binary()) ->
    ok | {error, not_implemented}.
mark_improvement_applied(_ImprovementId) ->
    {error, not_implemented}.
```

**File**: `apps/tcps_erlmcp/src/tcps_persistence.erl`
```erlang
% Line 820: Add spec for unimplemented function:
-spec execute_sparql_query(OntologyFiles :: [file:filename()],
                           Query :: binary()) ->
    {ok, Results :: [map()]} | {error, not_implemented}.
execute_sparql_query(_OntologyFiles, _Query) ->
    {error, not_implemented}.
```

**Verify**:
```bash
TERM=dumb rebar3 compile | grep -i "unused variable"
# Expected: No unused variable warnings for the 8 we fixed
```

#### 2.3: Fix Behaviour Conflict (1 CRITICAL)

**File**: `apps/erlmcp_transports/src/erlmcp_transport_http.erl`

```erlang
% Line 3 - Remove erlmcp_transport behaviour:
% Before:
-behaviour(erlmcp_transport).
-behaviour(gen_server).

% After:
-behaviour(gen_server).

%% ====================================================================
%% Transport Implementation Note
%% ====================================================================
%% This module implements the erlmcp_transport interface manually
%% rather than using -behaviour(erlmcp_transport) to avoid callback
%% conflicts with gen_server behaviour.
%%
%% The transport API (init/2, send/2, close/1) is implemented as
%% regular exported functions, not behaviour callbacks.
%% ====================================================================

% Update exports to clarify they're not behaviour callbacks:
-export([
    %% Transport API (manual implementation)
    init/2,
    send/2,
    close/1,

    %% gen_server callbacks
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).
```

**Verify**:
```bash
TERM=dumb rebar3 dialyzer 2>&1 | grep "erlmcp_transport_http"
# Expected: No behaviour conflict errors
```

---

### Step 3: Fix Medium-Priority Warnings ⏱️ 1 hour

#### 3.1: Fix OTP 27 Float Matching (4 occurrences)

**File**: `apps/tcps_erlmcp/src/tcps_cli_quality.erl`
```erlang
% Line 281:
# Manually edit:
vim apps/tcps_erlmcp/src/tcps_cli_quality.erl

% Before:
case Coverage of
    0.0 -> 0.0;
    C when C > 0 -> C
end.

% After:
case Coverage of
    +0.0 -> +0.0;  % Explicitly match positive zero only
    C when C > 0 -> C
end.
```

**File**: `apps/tcps_erlmcp/src/tcps_kaizen.erl`
```erlang
% Lines 936, 1137, 1144:
vim apps/tcps_erlmcp/src/tcps_kaizen.erl

% Replace all instances:
sed -i 's/\b0\.0\b/+0.0/g' apps/tcps_erlmcp/src/tcps_kaizen.erl
```

**Verify**:
```bash
TERM=dumb rebar3 compile | grep -i "float.*match"
# Expected: No float matching warnings
```

---

### Step 4: Add Missing Type Specs ⏱️ 4-6 hours

#### 4.1: Add Specs to erlmcp_server.erl

**File**: `apps/erlmcp_core/src/erlmcp_server.erl`

**Process**:
1. Read file to understand exports
2. Add -spec for each exported function
3. Add callback specs for gen_server

```bash
# Step 1: Identify exports
grep "^-export" apps/erlmcp_core/src/erlmcp_server.erl

# Step 2: Add specs (manual editing required)
vim apps/erlmcp_core/src/erlmcp_server.erl
```

**Template specs** (adapt based on actual exports):
```erlang
%% ====================================================================
%% API Function Specifications
%% ====================================================================

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
%% @doc Start MCP server with configuration map.
%% Config keys:
%%   - transport: Transport module (erlmcp_transport_stdio | erlmcp_transport_tcp | ...)
%%   - capabilities: Server capabilities (#{tools => #{}, resources => #{}, prompts => #{}})
%%   - server_info: Server metadata (name, version)

-spec stop(pid()) -> ok.
%% @doc Stop MCP server gracefully.

-spec add_tool(pid(), binary(), fun()) -> ok | {error, term()}.
%% @doc Add tool handler to server.
%% Handler function signature: fun((ToolName :: binary(), Args :: map()) ->
%%                                   {ok, Result :: map()} | {error, term()}).

-spec add_resource(pid(), binary(), fun()) -> ok | {error, term()}.
%% @doc Add resource handler to server.
%% Handler signature: fun((Uri :: binary()) -> {ok, Content :: map()} | {error, term()}).

-spec add_prompt(pid(), binary(), fun()) -> ok | {error, term()}.
%% @doc Add prompt template handler to server.

-spec call_tool(pid(), binary(), map()) -> {ok, map()} | {error, term()}.
%% @doc Execute tool with arguments.

-spec read_resource(pid(), binary()) -> {ok, map()} | {error, term()}.
%% @doc Read resource by URI.

-spec subscribe_resource(pid(), binary()) -> ok | {error, term()}.
%% @doc Subscribe to resource changes (MCP subscriptions).

%% ====================================================================
%% gen_server Callback Specifications
%% ====================================================================

-spec init([map()]) -> {ok, state()}.

-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: state()) ->
    {reply, Reply :: term(), NewState :: state()} |
    {reply, Reply :: term(), NewState :: state(), timeout() | hibernate} |
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: state()} |
    {stop, Reason :: term(), NewState :: state()}.

-spec handle_cast(Request :: term(), State :: state()) ->
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: state()}.

-spec handle_info(Info :: term(), State :: state()) ->
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: state()}.

-spec terminate(Reason :: term(), State :: state()) -> ok.

-spec code_change(OldVsn :: term(), State :: state(), Extra :: term()) ->
    {ok, NewState :: state()}.
```

**Verify**:
```bash
# Check spec coverage
grep "^-spec" apps/erlmcp_core/src/erlmcp_server.erl | wc -l
# Should be ~15+ specs
```

#### 4.2: Add Specs to erlmcp_client.erl

**File**: `apps/erlmcp_core/src/erlmcp_client.erl`

**Similar process**, add ~12 specs:

```erlang
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
-spec stop(pid()) -> ok.
-spec initialize(pid(), map()) -> {ok, map()} | {error, term()}.
-spec list_tools(pid()) -> {ok, [map()]} | {error, term()}.
-spec list_resources(pid()) -> {ok, [map()]} | {error, term()}.
-spec call_tool(pid(), binary(), map()) -> {ok, map()} | {error, term()}.
%% ... etc
```

#### 4.3: Document Error Functions in erlmcp_json_rpc.erl

**File**: `apps/erlmcp_core/src/erlmcp_json_rpc.erl`

**Add module documentation** (after -module declaration):

```erlang
%% ====================================================================
%% MCP 2025-11-25 Specification Error Codes
%% ====================================================================
%% This module implements ALL error codes from the MCP specification.
%% Error helper functions are kept for protocol compliance even if
%% not currently used, to support:
%%   1. Full MCP specification compliance
%%   2. Future feature additions
%%   3. Client library consistency
%%
%% Error Code Ranges:
%%   -32700 to -32600: JSON-RPC 2.0 standard errors
%%   -32010 to -32001: MCP core errors
%%   -32020 to -32011: Content and message errors
%%   -32030 to -32021: Resource and template errors
%%   -32040 to -32031: Tool and execution errors
%%   -32050 to -32041: Prompt and sampling errors
%%   -32060 to -32051: Authentication and authorization errors
%%   -32070 to -32061: Protocol and negotiation errors
%%   -32080 to -32071: Pagination and cursor errors
%%   -32090 to -32081: Task and job errors
%%   -32100 to -32091: Progress and notification errors
%%   -32113 to -32110: Completion errors
%%
%% Reference: https://spec.modelcontextprotocol.io/specification/2025-11-25/errors/
%% ====================================================================
```

**Add -doc attributes** to error functions (lines 480+):

```erlang
%% @doc MCP error -32011: Tool description exceeds maximum size limit
-doc """
MCP specification error code -32011: Tool description too large.
Returns a JSON-RPC error response indicating the tool description
exceeded the maximum allowed size.

Kept for MCP 2025-11-25 specification compliance.
""".
-spec error_tool_description_too_large(json_rpc_id(), pos_integer(), pos_integer()) -> binary().
error_tool_description_too_large(Id, ActualSize, MaxSize)
  when is_integer(ActualSize), is_integer(MaxSize) ->
    Data = #{
        <<"actualSize">> => ActualSize,
        <<"maxSize">> => MaxSize
    },
    encode_error_response(Id, ?MCP_ERROR_TOOL_DESCRIPTION_TOO_LONG,
                          ?MCP_MSG_TOOL_DESCRIPTION_TOO_LONG, Data).

%% Repeat for all 37 error functions...
```

**Note**: This is tedious but important for documentation. Can be scripted:

```bash
# Generate -doc attributes automatically
cat > /tmp/add_doc_attrs.sh <<'EOF'
#!/bin/bash
# Add -doc attributes to error functions in erlmcp_json_rpc.erl

FILE="apps/erlmcp_core/src/erlmcp_json_rpc.erl"

# For each error function, add -doc before -spec
sed -i '/-spec error_tool_description_too_large/i\
-doc """\
MCP specification error: Tool description too large.\
Kept for MCP 2025-11-25 specification compliance.\
""".' $FILE

# Repeat for other functions...
EOF

chmod +x /tmp/add_doc_attrs.sh
/tmp/add_doc_attrs.sh
```

---

### Step 5: Run Dialyzer ⏱️ 3-5 minutes

```bash
# 5.1: Rebuild PLT (first time is slow, ~45 seconds)
rebar3 dialyzer --update-plt

# Expected output:
# ===> Updating base PLT...
# ===> Analyzing 355 files...
# ===> Updating project PLT...
# ===> Analyzing 92 files...

# 5.2: Run full Dialyzer analysis
rebar3 dialyzer

# Expected output (success):
# ===> Analyzing applications...
# ===> Analyzing erlmcp_core...
# ===> Analyzing erlmcp_transports...
# ===> Analyzing erlmcp_observability...
# ===> Analyzing erlmcp_validation...
# ===> Dialyzer completed successfully with 0 warnings

# Or (if warnings remain):
# ===> Dialyzer found 5 warnings
#   erlmcp_templates.beam: Could not get Core Erlang code
#   tcps_kaizen.erl:1057: Unused variable _Waste
#   ... etc
```

**Interpreting Results**:

**Success Indicators**:
- ✅ "Dialyzer completed successfully with 0 warnings"
- ✅ No "Could not get Core Erlang code" errors (except erlmcp_templates.beam - known issue)
- ✅ No behaviour conflict errors
- ✅ No ambiguous BIF call warnings

**Acceptable Warnings** (can defer to future phases):
- Unused functions in TCPS modules (marked as TODO)
- Missing specs in non-critical modules
- erlmcp_templates.beam Core Erlang issue (known, <2% of codebase)

**Must-Fix Warnings** (blockers):
- Behaviour conflicts
- Type mismatch in callbacks
- Invalid function signatures
- Unmatched return types in critical paths

---

### Step 6: Fix Type Errors (if any) ⏱️ Varies (1-3 hours)

**If Dialyzer reports type errors**, analyze each:

**Example Error**:
```
erlmcp_server.erl:245: The call erlmcp_json_rpc:encode_response(Id, Result)
  breaks the contract (json_rpc_id(), term()) -> binary() | {error, term()}
```

**Fix Process**:
1. **Identify root cause**:
   - Look at line 245 in erlmcp_server.erl
   - Check what `Id` and `Result` are
   - Verify types match spec

2. **Fix the type mismatch**:
   ```erlang
   % Before (if Id might be undefined):
   Response = erlmcp_json_rpc:encode_response(Id, Result),

   % After (ensure Id is valid):
   ValidId = case Id of
       undefined -> null;
       _ -> Id
   end,
   Response = erlmcp_json_rpc:encode_response(ValidId, Result),
   ```

3. **Recompile and re-run Dialyzer**:
   ```bash
   TERM=dumb rebar3 compile
   rebar3 dialyzer
   ```

**Common Type Errors and Fixes**:

**Error**: "Record field might not exist"
```erlang
% Fix: Add type guard or use maps:get with default
Value = maps:get(field, Record, undefined)
```

**Error**: "Function returns {ok, term()} | {error, term()} but caller expects ok | {error, term()}"
```erlang
% Fix: Normalize return value
case some_function() of
    {ok, _} -> ok;
    Error -> Error
end
```

**Error**: "Invalid type specification for callback"
```erlang
% Fix: Match gen_server callback signature exactly
-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()}.  % Must match gen_server behaviour
```

---

### Step 7: Run Tests ⏱️ 2-3 minutes

```bash
# 7.1: Run unit tests
rebar3 eunit

# Expected output:
# ===> Performing EUnit tests...
# ................................................................
# .......................................... (100 tests, 0 failures)

# 7.2: Run Common Test suites
rebar3 ct

# Expected output:
# ===> Running Common Test suites...
# *** Test run completed ***
# PASSED: 150 tests, FAILED: 0 tests, SKIPPED: 0 tests

# 7.3: Check for any test failures
echo $?
# Expected: 0 (success)
```

**If tests fail**:
1. **Analyze failure** - Did our changes break something?
2. **Common causes**:
   - Type spec too restrictive (function returns more types than spec allows)
   - Behaviour callback signature mismatch
   - Missing export after renaming function
3. **Fix and re-test**

---

### Step 8: Update Production Profile ⏱️ 2 minutes

**File**: `/home/user/erlmcp/rebar.config`

**Lines 77-88**:
```erlang
% Before:
{prod, [
    {erl_opts, [
        no_debug_info,  % ❌ REMOVE THIS
        {d, 'PROD'}
    ]},
    ...
]}

% After:
{prod, [
    {erl_opts, [
        debug_info,  % ✅ KEEP debug_info in production
        {d, 'PROD'}
    ]},
    {compiler_warnings_as_errors, true},  % ✅ Enforce zero warnings in prod
    {relx, [
        {dev_mode, false},
        {include_erts, true},
        {include_src, false}
    ]}
]}
```

**Test production build**:
```bash
REBAR_PROFILE=prod rebar3 compile
REBAR_PROFILE=prod rebar3 dialyzer

# Expected: Same 0 warnings as dev build
```

---

## 9. Verification Checklist

### Pre-Implementation Checklist

- [ ] Read this entire document
- [ ] Understand current state from DIALYZER_REPORT.md
- [ ] Have access to erlmcp codebase
- [ ] Have rebar3 installed
- [ ] Have Erlang/OTP 25+ installed

### During Implementation

**After Step 1 (Clean Rebuild)**:
- [ ] rebar.config has `debug_info` on line 13
- [ ] `rm -rf _build` completed successfully
- [ ] `TERM=dumb rebar3 compile` completed with 0 errors
- [ ] All 146 modules (92+28+21+5) compiled successfully
- [ ] Random sample of beam files contain "Dbgi" chunk
- [ ] No "Could not get Core Erlang code" errors (except erlmcp_templates.beam - acceptable)

**After Step 2 (High-Priority Warnings)**:
- [ ] erlmcp_version.erl has `-compile({no_auto_import,[binary_to_integer/1]})` directive
- [ ] All 7 `binary_to_integer` calls in erlmcp_version.erl use `erlang:binary_to_integer`
- [ ] tcps_rebar3_receipt.erl calls `erlang:atom_to_binary(_, utf8)` (lines 238, 242)
- [ ] tcps_cli_receipt.erl calls `erlang:atom_to_binary(_, utf8)` (line 182)
- [ ] tcps_cli_format.erl calls `erlang:error(function_clause, _)` (line 109)
- [ ] tcps_cli_kaizen.erl line 45 has `{ok, _StartDate}`
- [ ] tcps_kaizen.erl has underscore-prefixed variables (lines 132, 840, 1057)
- [ ] tcps_kaizen.erl has specs for stub functions (lines 1209, 1214)
- [ ] tcps_persistence.erl line 820 has spec for `execute_sparql_query/2`
- [ ] erlmcp_transport_http.erl REMOVED `-behaviour(erlmcp_transport)` (line 3)
- [ ] erlmcp_transport_http.erl has documentation explaining manual transport implementation
- [ ] `TERM=dumb rebar3 compile` shows 0 ambiguous BIF warnings
- [ ] `TERM=dumb rebar3 compile` shows 0 behaviour conflict errors

**After Step 3 (Medium-Priority Warnings)**:
- [ ] tcps_cli_quality.erl line 281 uses `+0.0` instead of `0.0`
- [ ] tcps_kaizen.erl lines 936, 1137, 1144 use `+0.0` instead of `0.0`
- [ ] `TERM=dumb rebar3 compile` shows 0 float matching warnings

**After Step 4 (Type Specs)**:
- [ ] erlmcp_server.erl has ~15+ `-spec` declarations
- [ ] erlmcp_client.erl has ~12+ `-spec` declarations
- [ ] erlmcp_json_rpc.erl has module-level MCP documentation
- [ ] erlmcp_json_rpc.erl error functions have `-doc` attributes (or documented as MCP-required)
- [ ] `grep "^-spec" apps/erlmcp_core/src/erlmcp_server.erl | wc -l` returns >= 15
- [ ] `grep "^-spec" apps/erlmcp_core/src/erlmcp_client.erl | wc -l` returns >= 12

**After Step 5 (Dialyzer)**:
- [ ] `rebar3 dialyzer --update-plt` completed successfully
- [ ] PLT contains ~390+ files (355 stdlib + 37 project modules)
- [ ] `rebar3 dialyzer` completed without CRITICAL errors
- [ ] No "Could not get Core Erlang code" errors (except erlmcp_templates.beam - acceptable)
- [ ] No behaviour conflict warnings
- [ ] No ambiguous BIF call warnings
- [ ] No type mismatch errors in critical modules (server, client, json_rpc)

**After Step 6 (Type Errors - if any)**:
- [ ] All Dialyzer type errors resolved or documented as acceptable
- [ ] `rebar3 dialyzer` warnings count reduced to acceptable level
- [ ] No blocking type errors in core modules

**After Step 7 (Tests)**:
- [ ] `rebar3 eunit` passes with 0 failures
- [ ] `rebar3 ct` passes with 0 failures
- [ ] No test regressions introduced by type spec changes

**After Step 8 (Production Profile)**:
- [ ] rebar.config prod profile has `debug_info` (line 79)
- [ ] rebar.config prod profile has `{compiler_warnings_as_errors, true}`
- [ ] `REBAR_PROFILE=prod rebar3 compile` completes with 0 errors
- [ ] `REBAR_PROFILE=prod rebar3 dialyzer` shows same warnings as dev build

### Final Verification

**Quality Gate Metrics**:
- [ ] 0 compilation errors
- [ ] 0 critical Dialyzer warnings
- [ ] ≤10 medium-priority Dialyzer warnings (documented and tracked)
- [ ] 100% test pass rate
- [ ] Type spec coverage: erlmcp_server ≥90%, erlmcp_client ≥90%, erlmcp_json_rpc ≥80%
- [ ] All beam files have debug_info
- [ ] Production build works with debug_info enabled

**Documentation**:
- [ ] CHANGELOG.md updated with Phase 1c changes
- [ ] docs/TYPE_SPECS.md reviewed and confirmed accurate
- [ ] This implementation plan marked as COMPLETE

**Git Commit**:
- [ ] All changes committed with descriptive message
- [ ] Commit message includes "Phase 1c: Dialyzer & Type Safety Fixes"
- [ ] Commit message lists high-level changes (BIF fixes, behaviour conflicts, type specs added)

---

## 10. Type Spec Examples

### Example 1: gen_server Callbacks (erlmcp_server.erl)

```erlang
%% ====================================================================
%% gen_server Callbacks
%% ====================================================================

%% @doc Initialize MCP server state
-spec init([map()]) -> {ok, state()}.
init([Config]) ->
    process_flag(trap_exit, true),

    % Extract configuration
    Transport = maps:get(transport, Config),
    Capabilities = maps:get(capabilities, Config, #{}),
    ServerInfo = maps:get(server_info, Config, #{}),

    State = #state{
        transport = Transport,
        capabilities = Capabilities,
        server_info = ServerInfo,
        tools = #{},
        resources = #{},
        prompts = #{},
        subscriptions = #{}
    },

    logger:info("MCP server initialized with transport: ~p", [Transport]),
    {ok, State}.

%% @doc Handle synchronous calls
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: state()) ->
    {reply, Reply :: term(), NewState :: state()} |
    {noreply, NewState :: state()} |
    {stop, Reason :: term(), Reply :: term(), NewState :: state()}.
handle_call({add_tool, ToolName, Handler}, _From, State) ->
    NewTools = maps:put(ToolName, Handler, State#state.tools),
    NewState = State#state{tools = NewTools},
    {reply, ok, NewState};

handle_call({call_tool, ToolName, Args}, _From, State) ->
    case maps:get(ToolName, State#state.tools, undefined) of
        undefined ->
            {reply, {error, tool_not_found}, State};
        Handler ->
            Result = Handler(ToolName, Args),
            {reply, Result, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc Handle asynchronous casts
-spec handle_cast(Request :: term(), State :: state()) ->
    {noreply, NewState :: state()} |
    {stop, Reason :: term(), NewState :: state()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%% @doc Handle other messages
-spec handle_info(Info :: term(), State :: state()) ->
    {noreply, NewState :: state()} |
    {stop, Reason :: term(), NewState :: state()}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Cleanup on termination
-spec terminate(Reason :: term(), State :: state()) -> ok.
terminate(_Reason, _State) ->
    logger:info("MCP server terminating"),
    ok.

%% @doc Handle hot code upgrade
-spec code_change(OldVsn :: term(), State :: state(), Extra :: term()) ->
    {ok, NewState :: state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

### Example 2: Custom Types (erlmcp_server.erl)

```erlang
%% ====================================================================
%% Type Definitions
%% ====================================================================

-type tool_name() :: binary().
-type resource_uri() :: binary().
-type prompt_name() :: binary().

-type tool_handler() :: fun((tool_name(), map()) ->
                              {ok, map()} | {error, term()}).

-type resource_handler() :: fun((resource_uri()) ->
                                 {ok, map()} | {error, term()}).

-type prompt_handler() :: fun((prompt_name(), map()) ->
                               {ok, map()} | {error, term()}).

-type capabilities() :: #{
    tools => #{list_changed => boolean()},
    resources => #{subscribe => boolean(), list_changed => boolean()},
    prompts => #{list_changed => boolean()}
}.

-type server_info() :: #{
    name := binary(),
    version := binary()
}.

-type state() :: #state{
    transport :: module(),
    capabilities :: capabilities(),
    server_info :: server_info(),
    tools :: #{tool_name() => tool_handler()},
    resources :: #{resource_uri() => resource_handler()},
    prompts :: #{prompt_name() => prompt_handler()},
    subscriptions :: #{resource_uri() => [pid()]}
}.

-export_type([
    tool_name/0,
    resource_uri/0,
    prompt_name/0,
    tool_handler/0,
    resource_handler/0,
    prompt_handler/0,
    capabilities/0,
    server_info/0
]).
```

### Example 3: API Functions (erlmcp_server.erl)

```erlang
%% ====================================================================
%% API Functions
%% ====================================================================

%% @doc Start MCP server with configuration
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link(?MODULE, [Config], []).

%% @doc Stop MCP server
-spec stop(pid()) -> ok.
stop(ServerPid) ->
    gen_server:stop(ServerPid).

%% @doc Add tool handler to server
-spec add_tool(pid(), tool_name(), tool_handler()) -> ok | {error, term()}.
add_tool(ServerPid, ToolName, Handler) when is_binary(ToolName), is_function(Handler, 2) ->
    gen_server:call(ServerPid, {add_tool, ToolName, Handler}).

%% @doc Remove tool from server
-spec remove_tool(pid(), tool_name()) -> ok | {error, not_found}.
remove_tool(ServerPid, ToolName) when is_binary(ToolName) ->
    gen_server:call(ServerPid, {remove_tool, ToolName}).

%% @doc Call tool with arguments
-spec call_tool(pid(), tool_name(), map()) ->
    {ok, map()} | {error, tool_not_found | term()}.
call_tool(ServerPid, ToolName, Args) when is_binary(ToolName), is_map(Args) ->
    gen_server:call(ServerPid, {call_tool, ToolName, Args}, 30000).

%% @doc Add resource handler to server
-spec add_resource(pid(), resource_uri(), resource_handler()) ->
    ok | {error, term()}.
add_resource(ServerPid, Uri, Handler) when is_binary(Uri), is_function(Handler, 1) ->
    gen_server:call(ServerPid, {add_resource, Uri, Handler}).

%% @doc Read resource by URI
-spec read_resource(pid(), resource_uri()) ->
    {ok, map()} | {error, resource_not_found | term()}.
read_resource(ServerPid, Uri) when is_binary(Uri) ->
    gen_server:call(ServerPid, {read_resource, Uri}, 10000).

%% @doc Subscribe to resource changes
-spec subscribe_resource(pid(), resource_uri()) ->
    ok | {error, resource_not_found | subscription_not_supported}.
subscribe_resource(ServerPid, Uri) when is_binary(Uri) ->
    gen_server:call(ServerPid, {subscribe_resource, Uri}).

%% @doc Unsubscribe from resource changes
-spec unsubscribe_resource(pid(), resource_uri()) -> ok.
unsubscribe_resource(ServerPid, Uri) when is_binary(Uri) ->
    gen_server:call(ServerPid, {unsubscribe_resource, Uri}).
```

### Example 4: Error Handling Types (erlmcp_json_rpc.erl)

```erlang
%% ====================================================================
%% Error Handling Types
%% ====================================================================

-type error_code() :: integer().
-type error_message() :: binary().
-type error_data() :: map() | binary() | undefined.

-type mcp_error() :: #{
    code := error_code(),
    message := error_message(),
    data => error_data()
}.

-type content_error_reason() ::
    tool_description_too_large |
    invalid_content_type |
    content_too_large |
    invalid_encoding.

-type resource_error_reason() ::
    resource_not_found |
    resource_template_not_found |
    invalid_uri |
    uri_syntax_error |
    resource_access_denied |
    template_render_failed.

-type tool_error_reason() ::
    tool_not_found |
    tool_execution_failed |
    tool_timeout |
    tool_cancelled |
    invalid_tool_arguments.

-export_type([
    error_code/0,
    error_message/0,
    error_data/0,
    mcp_error/0,
    content_error_reason/0,
    resource_error_reason/0,
    tool_error_reason/0
]).
```

### Example 5: Complex Map Types (erlmcp_client.erl)

```erlang
%% ====================================================================
%% Client Types
%% ====================================================================

-type client_info() :: #{
    name := binary(),
    version := binary()
}.

-type server_capabilities() :: #{
    tools => #{
        list_changed => boolean()
    },
    resources => #{
        subscribe => boolean(),
        list_changed => boolean()
    },
    prompts => #{
        list_changed => boolean()
    },
    logging => #{
        levels => [binary()]
    }
}.

-type initialize_result() :: #{
    protocol_version := binary(),
    capabilities := server_capabilities(),
    server_info := #{
        name := binary(),
        version := binary()
    }
}.

-type tool_info() :: #{
    name := binary(),
    description => binary(),
    input_schema := map()
}.

-type resource_info() :: #{
    uri := binary(),
    name := binary(),
    description => binary(),
    mime_type => binary()
}.

-type prompt_info() :: #{
    name := binary(),
    description => binary(),
    arguments => [#{
        name := binary(),
        description => binary(),
        required => boolean()
    }]
}.

%% ====================================================================
%% Client API Functions
%% ====================================================================

-spec initialize(pid(), client_info()) ->
    {ok, initialize_result()} | {error, term()}.
initialize(ClientPid, ClientInfo) when is_map(ClientInfo) ->
    gen_server:call(ClientPid, {initialize, ClientInfo}, 30000).

-spec list_tools(pid()) ->
    {ok, [tool_info()]} | {error, term()}.
list_tools(ClientPid) ->
    gen_server:call(ClientPid, list_tools).

-spec list_resources(pid()) ->
    {ok, [resource_info()]} | {error, term()}.
list_resources(ClientPid) ->
    gen_server:call(ClientPid, list_resources).

-spec list_prompts(pid()) ->
    {ok, [prompt_info()]} | {error, term()}.
list_prompts(ClientPid) ->
    gen_server:call(ClientPid, list_prompts).
```

---

## 11. Timeline

### Estimated Time Breakdown

| Step | Task | Estimated Time | Difficulty |
|------|------|----------------|------------|
| 1 | Clean rebuild with debug_info | 5 minutes | Easy |
| 2.1 | Fix ambiguous BIF calls (11 occurrences) | 30 minutes | Easy |
| 2.2 | Fix unused variables (8 occurrences) | 20 minutes | Easy |
| 2.3 | Fix behaviour conflict (1 critical) | 45 minutes | Medium |
| 3 | Fix OTP 27 float matching (4 occurrences) | 30 minutes | Easy |
| 4.1 | Add specs to erlmcp_server.erl (~15 specs) | 2 hours | Medium |
| 4.2 | Add specs to erlmcp_client.erl (~12 specs) | 1.5 hours | Medium |
| 4.3 | Document error functions in erlmcp_json_rpc.erl | 1 hour | Easy |
| 5 | Run Dialyzer and analyze results | 5 minutes | Easy |
| 6 | Fix type errors (if any) | 1-3 hours | Hard |
| 7 | Run tests and verify | 3 minutes | Easy |
| 8 | Update production profile | 2 minutes | Easy |
| - | **Total** | **8-12 hours** | **Mixed** |

### Recommended Schedule

**Day 1 (4-5 hours)**:
- Morning: Steps 1-3 (Clean rebuild + High/Medium priority warnings)
  - Clean rebuild: 5 min
  - Fix BIF calls: 30 min
  - Fix unused vars: 20 min
  - Fix behaviour conflict: 45 min
  - Fix float matching: 30 min
  - **Total**: ~2.5 hours

- Afternoon: Step 4.1 (Add specs to erlmcp_server.erl)
  - Read module structure: 20 min
  - Add API specs: 1 hour
  - Add callback specs: 40 min
  - **Total**: 2 hours

**Day 2 (4-5 hours)**:
- Morning: Steps 4.2-4.3 (Remaining type specs)
  - erlmcp_client.erl specs: 1.5 hours
  - erlmcp_json_rpc.erl documentation: 1 hour
  - **Total**: 2.5 hours

- Afternoon: Steps 5-8 (Dialyzer + fixes + verification)
  - Run Dialyzer: 5 min
  - Fix type errors (if any): 1-3 hours
  - Run tests: 3 min
  - Update prod profile: 2 min
  - Final verification: 10 min
  - **Total**: 1.5-3.5 hours

**Total**: 8-12 hours (1-1.5 developer days)

### Parallel Work Opportunities

**Can be parallelized**:
- Steps 2.1, 2.2, 2.3 can be done by different developers simultaneously
- Steps 4.1, 4.2, 4.3 can be done by different developers simultaneously

**Must be sequential**:
- Step 1 must complete before Step 2
- Steps 2-4 must complete before Step 5 (Dialyzer)
- Step 5 must complete before Step 6 (fix type errors)
- Step 6 must complete before Step 7 (tests)

---

## 12. Troubleshooting

### Issue: Dialyzer hangs or takes too long

**Symptoms**: `rebar3 dialyzer` runs for >10 minutes without progress

**Causes**:
1. Large PLT rebuild
2. Circular type dependencies
3. Infinite type recursion

**Solutions**:
```bash
# 1. Kill dialyzer and remove old PLT
pkill -9 dialyzer
rm -rf _build/default/*_plt

# 2. Rebuild PLT incrementally
rebar3 dialyzer --update-plt

# 3. If still hangs, try without incremental PLT
rm ~/.cache/rebar3/*_plt
rebar3 dialyzer
```

### Issue: "Could not get Core Erlang code" for many modules

**Symptoms**: Dialyzer reports 10+ modules without Core Erlang code

**Causes**:
1. Beam files compiled without debug_info
2. Stale build artifacts
3. Production profile used accidentally

**Solutions**:
```bash
# 1. Verify rebar.config has debug_info (line 13)
grep "debug_info" rebar.config

# 2. Clean rebuild
rm -rf _build
unset REBAR_PROFILE  # Ensure not using 'prod' profile
TERM=dumb rebar3 compile

# 3. Verify debug_info in beam files
strings _build/default/lib/erlmcp_core/ebin/erlmcp_auth.beam | grep "Dbgi"
# Should output: Dbgi (or similar)
```

### Issue: Tests fail after adding type specs

**Symptoms**: `rebar3 eunit` reports test failures after type spec changes

**Causes**:
1. Type spec too restrictive (doesn't match actual function behavior)
2. Test code violates type contract
3. Missing export after function rename

**Solutions**:
```bash
# 1. Run specific failing test
rebar3 eunit --module=erlmcp_server_tests --test=add_tool_test

# 2. Check error message
# Example: "Function returned {ok, undefined} but spec says {ok, binary()}"

# 3. Fix spec to match actual behavior
-spec add_tool(...) -> {ok, term()} | {error, term()}.  % More permissive
# OR
-spec add_tool(...) -> {ok, binary() | undefined} | {error, term()}.  % Explicit

# 4. Re-run tests
rebar3 eunit
```

### Issue: New Dialyzer warnings appear after fixes

**Symptoms**: After fixing 11 BIF ambiguity warnings, 5 new type warnings appear

**Causes**:
1. Fixing one issue exposes deeper type issues
2. Type inference now works, reveals actual type mismatches

**Solutions**:
1. **Analyze each new warning**:
   ```bash
   rebar3 dialyzer 2>&1 | tee dialyzer_warnings.txt
   vim dialyzer_warnings.txt
   ```

2. **Categorize warnings**:
   - **Critical**: Must fix (type mismatch in core logic)
   - **Medium**: Should fix (unused functions, unclear types)
   - **Low**: Can defer (overly-general specs)

3. **Fix systematically**:
   ```bash
   # For each critical warning:
   # 1. Understand the type mismatch
   # 2. Fix the code OR adjust the spec
   # 3. Re-run Dialyzer
   # 4. Verify tests still pass
   ```

### Issue: Behaviour conflict persists after removing -behaviour declaration

**Symptoms**: After removing `-behaviour(erlmcp_transport)`, Dialyzer still complains

**Causes**:
1. Beam file not recompiled (stale artifact)
2. Callback still exported with same signature

**Solutions**:
```bash
# 1. Clean rebuild
rm -rf _build/default/lib/erlmcp_transports
TERM=dumb rebar3 compile

# 2. Verify behaviour not in beam metadata
beam_lib:chunks('_build/default/lib/erlmcp_transports/ebin/erlmcp_transport_http.beam', [attributes]).
# Should NOT list erlmcp_transport in behaviours

# 3. Rename conflicting callback if needed
% Before:
init(Type, Opts) ->  % Conflicts with gen_server:init/1

% After:
transport_init(Type, Opts) ->  % Distinct name, no conflict
```

### Issue: Type error in generated code

**Symptoms**: Dialyzer reports type error in code we didn't write (e.g., parse transforms, behaviours)

**Example**:
```
erlmcp_templates.beam: Could not get Core Erlang code
```

**Causes**:
1. bbmustache or other dependency uses parse transforms
2. Complex macros expand to code Dialyzer can't analyze
3. Behaviour module has incompatible OTP version

**Solutions**:
1. **Acceptable workaround**: Exclude from Dialyzer analysis
   ```erlang
   % In rebar.config:
   {dialyzer, [
       {exclude_mods, [erlmcp_templates]}  % <2% of codebase, acceptable
   ]}.
   ```

2. **Long-term fix**: Simplify code to avoid parse transforms
   ```erlang
   % Instead of complex bbmustache templates:
   % Use simpler binary concatenation or io_lib:format
   ```

3. **Document as known issue**:
   ```erlang
   %% @doc Template rendering module
   %% NOTE: This module uses bbmustache parse transforms which
   %% prevent Dialyzer from extracting Core Erlang code.
   %% This is a known limitation affecting <2% of the codebase.
   ```

### Issue: Production build fails with "warnings_as_errors"

**Symptoms**: `REBAR_PROFILE=prod rebar3 compile` fails with warnings

**Causes**:
1. Production profile has `{compiler_warnings_as_errors, true}`
2. Dev build has warnings suppressed, prod build doesn't

**Solutions**:
```bash
# 1. Find which warnings are blocking
REBAR_PROFILE=prod TERM=dumb rebar3 compile 2>&1 | grep "Warning:"

# 2. Fix warnings (don't suppress them in prod!)
# Example: Unused variable -> prefix with underscore
# Example: Missing spec -> add -spec

# 3. Alternatively, relax prod warnings temporarily (NOT RECOMMENDED)
# In rebar.config prod profile:
{compiler_warnings_as_errors, false}  % Only for emergency deploys
```

### Issue: Spec coverage still low after adding specs

**Symptoms**: Added 15 specs to erlmcp_server.erl, but coverage report shows 60%

**Causes**:
1. Internal helper functions lack specs
2. Private functions counted in coverage metric
3. Generated functions (from behaviours) not counted

**Solutions**:
```bash
# 1. Count exported function specs vs total exports
grep "^-export" apps/erlmcp_core/src/erlmcp_server.erl
# Output: -export([start_link/1, stop/1, add_tool/3, ...]).  # 15 functions

grep "^-spec.*-> " apps/erlmcp_core/src/erlmcp_server.erl | wc -l
# Output: 15  # All exported functions have specs ✅

# 2. For 100% coverage, add specs to internal functions too
-spec internal_helper(term()) -> term().

# 3. Use Dialyzer to verify
rebar3 dialyzer --apps erlmcp_core
# Should show 0 "missing spec" warnings
```

### Issue: Dialyzer reports type mismatch in map keys

**Symptoms**: "Map key 'field_name' might not exist"

**Example**:
```
erlmcp_server.erl:123: Map key 'capabilities' might not exist
```

**Causes**:
1. Pattern matching on optional map key
2. Using `maps:get/2` without default

**Solutions**:
```erlang
% Before (unsafe):
Capabilities = maps:get(capabilities, Config),  % Crashes if key missing

% After Option 1 (with default):
Capabilities = maps:get(capabilities, Config, #{}),

% After Option 2 (with case):
Capabilities = case maps:get(capabilities, Config, undefined) of
    undefined -> #{};  % Default capabilities
    Caps -> Caps
end,

% After Option 3 (update type spec):
-type config() :: #{
    capabilities := map(),  % Required key (:=)
    server_info => map()    % Optional key (=>)
}.
```

---

## Conclusion

This implementation plan provides a comprehensive, step-by-step guide to achieving Phase 1c completion: Dialyzer & Type Safety Fixes.

**Success Criteria**:
- ✅ 0 critical Dialyzer warnings
- ✅ ≤10 medium-priority Dialyzer warnings (documented)
- ✅ Type spec coverage ≥90% for core modules (erlmcp_server, erlmcp_client)
- ✅ Type spec coverage ≥80% for erlmcp_json_rpc
- ✅ All beam files have debug_info
- ✅ 100% test pass rate
- ✅ Production build works with debug_info enabled

**Estimated Time**: 8-12 hours (1-1.5 developer days)

**Priority**: High - Blocks production deployment, compromises type safety

**Next Steps After Completion**:
1. Update CHANGELOG.md with Phase 1c changes
2. Create git commit with descriptive message
3. Run full CI/CD pipeline to verify no regressions
4. Proceed to Phase 1d (if applicable) or Phase 2

---

**Document Version**: 1.0
**Created**: 2026-01-31
**Author**: Erlang Architect Agent
**Status**: Ready for Implementation
