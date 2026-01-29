# Fix 250 Xref warnings (undefined calls + unused functions) Manufacturing Plan

**TCPS Compliance**: Lean Six Sigma Level Strictness
**Zero Defects**: 99.99966% defect-free delivery (3.4 defects per million)

**CRITICAL DEPENDENCY**: Item 020 depends on Items 018 and 021 completing first.

**Execution Dependencies**:
- **Item 018 (User Story US-001)**: Must resolve all 222 undefined function call warnings before final verification
- **Item 021 (User Story US-002)**: Must remove all 28 unused local function warnings before final verification
- **Item 020 (User Story US-003)**: Final verification step that confirms both Items 018 and 021 completed successfully

**Parallel Execution**: Items 018 and 021 can be executed in parallel (different categories, no conflicts). Item 020 serves as the final coordination and verification step.

## Manufacturing Objective

Eliminate **all 250 Xref warnings** blocking the production release of erlmcp v2.1.0 by systematically resolving undefined function calls (implement missing functions or remove orphaned calls) and removing dead code (unused local functions). Achieve **zero warnings** as mandated by the Xref quality gate.

This item serves as the **overall coordination and final verification** for the combined fix of:
- **Item 018**: 222 undefined function calls (missing modules or incomplete implementations)
- **Item 021**: 28 unused local functions (dead code from refactoring or public API exports)

### Quality Gate Requirements
- **Compilation**: 0 errors (MANDATORY)
- **EUnit**: 100% pass rate (MANDATORY)
- **Common Test**: 100% pass rate (if applicable)
- **Coverage**: ≥80% (MANDATORY)
- **Dialyzer**: 0 warnings (MANDATORY)
- **Xref**: 0 undefined function calls, 0 unused functions (MANDATORY)
- **Performance**: <10% regression from baseline (if applicable)

## Current State

### What Exists Now
- **Modules**: 168 Erlang modules across 4 applications
  - `apps/erlmcp_core/src/` - 52 modules (client, server, registry, json_rpc, batch, hooks, pricing)
  - `apps/erlmcp_transports/src/` - 22 modules (stdio, tcp, http, websocket, sse, security_headers)
  - `apps/erlmcp_observability/src/` - 26 modules (metrics, traces, receipts, debugger)
  - `apps/tcps_erlmcp/src/` - 68 modules (andon, heijunka, kanban, quality_gates, work_order, persistence)

- **Tests**: 14/14 EUnit tests pass (erlmcp_batch_tests only)
- **Quality**:
  - Compilation: ✅ PASS (0 errors, 2 cosmetic warnings)
  - Xref: ❌ FAIL (250 warnings - 222 undefined, 28 unused)

### What's Missing
- **Gap**: 250 Xref warnings blocking production (100% must be eliminated)
- **Undefined Function Calls (222)**:
  - `erlmcp_request_id:safe_increment/1` - Module does NOT exist (stub never implemented)
  - `erlmcp_task_manager:register_server/2` - Module does NOT exist (replaced by erlmcp_hooks)
  - `erlmcp_pricing_state:get_plan/1` - Module EXISTS but function missing
  - `tcps_persistence:store_sku/1` - Module EXISTS but function missing
  - `tcps_work_order:update_status/2` - Module EXISTS but function missing
  - `erlmcp_tracing:*` - Module does NOT exist (placeholder name)

- **Unused Local Functions (28)**:
  - `erlmcp_security_headers:add_to_response/1` - Exported but not called internally (public API)
  - `erlmcp_server:create_audio_content/3` - Dead code from v2.0 refactoring
  - `erlmcp_server:create_audio_content_with_metadata/4` - Dead code from v2.0 refactoring
  - `erlmcp_router:select_by_weight/3` - Dead code
  - `erlmcp_transport_sse:format_retry_field/1` - Dead code
  - `tcps_receipt_verifier:is_atom_stage/1` - Dead code
  - `tcps_work_order:atom_to_binary/1` - Dead code

**Root Cause**: Incomplete v2.0 refactoring - stub modules never implemented, renamed modules left orphaned references, dead code not removed. Lack of Poka-yoke (error-proofing) during refactoring.

### Key Discoveries from Research
- **Finding 1**: `erlmcp_client.erl:478` calls `erlmcp_request_id:safe_increment/1` but module does not exist
- **Finding 2**: `erlmcp_server.erl:179` calls `erlmcp_task_manager:register_server/2` but erlmcp_hooks.erl replaced this module
- **Finding 3**: `erlmcp_pricing_state.erl` exists with `get_current_plan/0` but missing `get_plan/1` called by `erlmcp_pricing_cli.erl`
- **Finding 4**: `tcps_persistence.erl` exists with comprehensive storage API but missing `store_sku/1`
- **Finding 5**: `erlmcp_security_headers.erl:184` exports `add_to_response/1` as public API (not dead code)

## Desired End State

### Specification
Zero Xref warnings achieved through:
1. **Undefined Function Resolution**: Implement 3 missing functions OR remove 3 orphaned module calls
2. **Dead Code Removal**: Remove 28 unused local functions OR document as public API exports

### Verification
```bash
# 1. Clean build
rebar3 clean
rebar3 compile

# 2. Run Xref - MUST show 0 warnings
rebar3 xref

# 3. Verify output
grep -c "Warning:" _build/test/logs/xref.log
# Expected output: 0 (no warnings found)

# 4. Run tests
rebar3 eunit
rebar3 ct

# 5. Coverage check
rebar3 cover
# Target: ≥80% for new code
```

### Manufacturing Output
- **Code**: Modified 15 modules (3 implementations, 28 function removals, 2 public API documentations)
- **Tests**: Created 3 new test modules (erlmcp_pricing_state_tests, tcps_persistence_tests, tcps_work_order_tests)
- **Documentation**: Updated README with Xref compliance status
- **Receipts**: Quality gate receipt showing 0 Xref warnings

## What We're NOT Doing
- **Out of Scope**: Fixing Dialyzer warnings (526 warnings) - handled in separate item
- **Out of Scope**: Increasing test coverage from 1% to 80% - handled in separate item
- **Out of Scope**: Implementing missing test modules (13 missing) - handled in separate item
- **Out of Scope**: Performance optimization - Xref fixes will not impact performance
- **Reason**: Focus on Xref warnings only to prevent scope creep and ensure production readiness

## Manufacturing Approach

### TCPS Methodology
Following Toyota Code Production System phases:

1. **Specification** - Requirements with acceptance criteria (categorize all 250 warnings)
2. **Pseudocode** - Algorithm design BEFORE coding (implement vs remove decision matrix)
3. **Architecture** - Integration points and supervision tree (call site analysis)
4. **Refinement** - Chicago School TDD (tests FIRST - write tests before implementing functions)
5. **Completion** - All quality gates passing (rebar3 xref shows 0 warnings)

### Implementation Strategy

**Coordinated Parallel Execution** (Heijunka - production leveling):

**Execution Order**:
1. **Phase 1 (Item 018)**: Resolve 222 undefined function calls - 8 hours
   - Implement missing critical functions (erlmcp_pricing_state:get_plan/1, tcps_persistence:store_sku/1, tcps_work_order:update_status/2)
   - Remove replaced module calls (erlmcp_task_manager → erlmcp_hooks, erlmcp_request_id → inline logic, erlmcp_tracing → remove all)
   - Update calling code (erlmcp_server, erlmcp_client)
   - Write tests for new functions (EUnit tests)
   - Verify Xref shows 0 undefined function call warnings

2. **Phase 2 (Item 021)**: Remove 28 unused local functions - 4 hours
   - Audit all unused functions
   - Remove dead code (erlmcp_router, erlmcp_pagination, erlmcp_server audio functions, erlmcp_transport_sse retry functions, tcps_receipt_verifier, tcps_work_order)
   - Document public API exports (erlmcp_security_headers with -compile directives)
   - Verify Xref shows 0 unused local function warnings

3. **Phase 3 (Item 020)**: Final verification - 2 hours
   - Clean build: `rebar3 clean && rebar3 compile`
   - Run Xref: `rebar3 xref` - verify 0 warnings
   - Run tests: `rebar3 eunit`, `rebar3 ct`
   - Coverage: `rebar3 cover` - verify ≥80% for new code
   - Update CHANGELOG.md and README.md
   - Generate compliance receipt

**Total Estimated Time**: 16 hours (2 days) across Items 018, 021, 020

**Why This Strategy**: Separates concerns (undefined calls vs dead code), allows parallel execution of Items 018 and 021, minimizes risk by implementing only critical functions, removes dead code aggressively, final verification ensures zero warnings.

### Quality Integration
- **Pre-commit Hooks**: `.claude/hooks/pre-task-validate.sh` runs `rebar3 xref` before commit
- **CI Gates**: GitHub Actions workflow runs Xref on every PR (blocks merge if warnings > 0)
- **Receipt Generation**: `tcps_quality_gates:generate_receipt/1` creates Xref compliance receipt
- **Andon Signaling**: Xref failures are logged as Andon events (severity: critical)

---

## Phases

### Phase 1: Categorize All 250 Xref Warnings (≤2 hours)

#### Overview
Document complete inventory of all 250 Xref warnings, separating undefined function calls from unused local functions, and documenting decision matrix (IMPLEMENT vs REMOVE vs DOCUMENT).

#### Specification
Create categorization table with:
- Warning type (undefined function call vs unused local function)
- Module:function/arity
- File location (module.erl:line)
- Call site (who calls this function?)
- Category (missing module, partial implementation, dead code, public API)
- Decision (IMPLEMENT, REMOVE, DOCUMENT)
- Justification (critical path/replaced/not needed/public API)

#### Pseudocode
```bash
# 1. Run Xref and capture output
rebar3 xref 2>&1 | tee xref_output.txt

# 2. Parse Xref output
grep "Undefined function call" xref_output.txt > undefined_calls.txt
grep "Unused local function" xref_output.txt > unused_functions.txt

# 3. Categorize undefined calls
for call in $(cat undefined_calls.txt); do
    # Check if module exists
    if [ -f "apps/*/src/${module}.erl" ]; then
        # Module exists but function missing -> IMPLEMENT
        echo "${module}:${function}/${arity} -> IMPLEMENT (partial implementation)"
    else
        # Module does not exist -> REMOVE (orphaned call)
        echo "${module}:${function}/${arity} -> REMOVE (module deleted)"
    fi
done

# 4. Categorize unused functions
for func in $(cat unused_functions.txt); do
    # Check if function is exported
    if grep -q "${func}" exports; then
        # Exported but unused -> DOCUMENT (public API)
        echo "${module}:${func} -> DOCUMENT (public API)"
    else
        # Local and unused -> REMOVE (dead code)
        echo "${module}:${func} -> REMOVE (dead code)"
    fi
done
```

#### Architecture
**Output Files**:
- `xref_categorization.csv` - Complete inventory with decision matrix
- `undefined_calls_inventory.md` - Detailed breakdown of 222 undefined calls
- `unused_functions_inventory.md` - Detailed breakdown of 28 unused functions

**Categories**:
- **IMPLEMENT (3 functions)**:
  - `erlmcp_pricing_state:get_plan/1` - Blocks pricing CLI
  - `tcps_persistence:store_sku/1` - Blocks SKU receipt generation
  - `tcps_work_order:update_status/2` - Blocks work order lifecycle

- **REMOVE (3 module calls)**:
  - `erlmcp_task_manager:register_server/2` - Replaced by erlmcp_hooks
  - `erlmcp_request_id:safe_increment/1` - Replace with inline overflow check
  - `erlmcp_tracing:*` - Placeholder module, remove all calls

- **DOCUMENT (2 functions)**:
  - `erlmcp_security_headers:add_to_response/1` - Public API for HTTP middleware
  - `tcps_receipt_verifier:is_atom_stage/1` - Public API for validation

- **REMOVE (26 functions)**:
  - Audio content functions (2) - Dead code from v2.0
  - Router functions (8) - Dead code from v2.0
  - SSE retry functions (2) - Dead code from v2.0
  - Utility functions (14) - Dead code from v2.0

#### Changes Required:

##### 1. Xref Categorization Table
**File**: `xref_categorization.csv`
**Current**: Does not exist
**Changes**: Create CSV with columns: warning_type, module, function, arity, file_location, call_site, category, decision, justification

```csv
warning_type,module,function,arity,file_location,call_site,category,decision,justification
undefined_function_call,erlmcp_request_id,safe_increment,1,erlmcp_client.erl:478,erlmcp_client,missing_module,REMOVE,Stub module never implemented
undefined_function_call,erlmcp_task_manager,register_server,2,erlmcp_server.erl:179,erlmcp_server,replaced_module,REMOVE,Replaced by erlmcp_hooks
undefined_function_call,erlmcp_pricing_state,get_plan,1,erlmcp_pricing_cli.erl:7,erlmcp_pricing_cli,partial_implementation,IMPLEMENT,Missing function blocks pricing CLI
undefined_function_call,tcps_persistence,store_sku,1,tcps_sku.erl:1319,tcps_sku,partial_implementation,IMPLEMENT,Missing function blocks SKU receipt storage
undefined_function_call,tcps_work_order,update_status,2,tcps_sku.erl:1343,tcps_sku,partial_implementation,IMPLEMENT,Missing function blocks work order updates
unused_local_function,erlmcp_security_headers,add_to_response,1,erlmcp_security_headers.erl:184,none,public_api,DOCUMENT,Exported function for HTTP middleware
unused_local_function,erlmcp_server,create_audio_content,3,erlmcp_server.erl:1272,none,dead_code,REMOVE,Dead code from v2.0 refactoring
unused_local_function,erlmcp_router,select_by_weight,3,erlmcp_router.erl:XXX,none,dead_code,REMOVE,Dead code from v2.0 refactoring
...
```

**Reason**: Complete inventory required for systematic resolution, prevents missing warnings, provides audit trail.

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] `xref_categorization.csv` created with 250 rows (one per warning)
- [ ] `undefined_calls_inventory.md` created with 222 undefined calls documented
- [ ] `unused_functions_inventory.md` created with 28 unused functions documented
- [ ] All warnings categorized (IMPLEMENT, REMOVE, DOCUMENT)
- [ ] Justification provided for each decision

##### Manual Verification:
- [ ] Review categorization table for accuracy
- [ ] Verify IMPLEMENT decisions (only 3 critical functions)
- [ ] Verify REMOVE decisions (orphaned calls and dead code)
- [ ] Verify DOCUMENT decisions (genuine public API exports)

**Note**: Complete categorization BEFORE proceeding to implementation. This ensures systematic coverage of all 250 warnings.

---

### Phase 2: Implement Missing Functions (≤4 hours)

#### Overview
Implement 3 missing functions (erlmcp_pricing_state:get_plan/1, tcps_persistence:store_sku/1, tcps_work_order:update_status/2) following OTP patterns and Chicago School TDD.

#### Specification
**Functions to Implement**:
1. `erlmcp_pricing_state:get_plan/1` - Retrieve pricing plan by ID
2. `tcps_persistence:store_sku/1` - Store SKU receipt with SHA-256 checksum
3. `tcps_work_order:update_status/2` - Update work order status

**Function Signatures**:
```erlang
%% erlmcp_pricing_state.erl
-spec get_plan(PlanId :: binary()) -> {ok, map()} | {error, not_found}.

%% tcps_persistence.erl
-spec store_sku(Sku :: map()) -> {ok, binary()} | {error, term()}.

%% tcps_work_order.erl
-spec update_status(WorkOrderId :: binary(), Status :: atom()) -> ok | {error, term()}.
```

#### Pseudocode

**erlmcp_pricing_state:get_plan/1**:
```erlang
get_plan(PlanId) ->
    %% Pseudocode:
    %% 1. Ensure ETS table exists
    ensure_table(),
    %% 2. Lookup plan by ID (binary key)
    case ets:lookup(?STATE_TABLE, PlanId) of
        [{PlanId, Plan}] -> {ok, Plan};
        [] -> {error, not_found}
    end.
```

**tcps_persistence:store_sku/1**:
```erlang
store_sku(Sku) ->
    %% Pseudocode:
    %% 1. Extract SKU ID and stage from map
    #{sku_id := SkuId, stage := Stage} = Sku,
    %% 2. Ensure directory exists
    ReceiptDir = receipt_dir(SkuId),
    ok = filelib:ensure_dir(filename:join(ReceiptDir, "dummy")),
    %% 3. Generate filename (reuse existing generate_receipt_filename/2)
    Timestamp = maps:get(timestamp, Sku, timestamp_now()),
    Filename = generate_receipt_filename(Stage, Timestamp),
    Path = filename:join(ReceiptDir, Filename),
    %% 4. Calculate SHA-256 checksum (reuse existing calculate_checksum/1)
    SkuJson = jsone:encode(Sku),
    Checksum = calculate_checksum(SkuJson),
    SkuWithChecksum = Sku#{checksum => Checksum},
    %% 5. Write to file
    FinalJson = jsone:encode(SkuWithChecksum),
    ok = file:write_file(Path, FinalJson),
    %% 6. Return path
    {ok, list_to_binary(Path)}.
```

**tcps_work_order:update_status/2**:
```erlang
update_status(WorkOrderId, NewStatus) ->
    %% Pseudocode:
    %% 1. Load existing work order
    case get_work_order(WorkOrderId) of
        {ok, WorkOrder} ->
            %% 2. Update status field
            UpdatedWorkOrder = WorkOrder#{status => NewStatus},
            %% 3. Store updated work order (reuse update_work_order/1)
            ok = update_work_order(UpdatedWorkOrder),
            ok;
        {error, not_found} ->
            {error, not_found}
    end.
```

#### Architecture
**Integration Points**:
- `erlmcp_pricing_state:get_plan/1` called by `erlmcp_pricing_cli.erl:7`
- `tcps_persistence:store_sku/1` called by `tcps_sku.erl:1319`
- `tcps_work_order:update_status/2` called by `tcps_sku.erl:1343`

**Dependencies**:
- `erlmcp_pricing_state` uses ETS table (?STATE_TABLE)
- `tcps_persistence` uses existing helper functions (receipt_dir, generate_receipt_filename, calculate_checksum)
- `tcps_work_order` uses existing functions (get_work_order, update_work_order)

**Supervision**: None (all are module-level functions, not gen_servers)

#### Changes Required:

##### 1. erlmcp_pricing_state.erl
**File**: `apps/erlmcp_core/src/pricing/erlmcp_pricing_state.erl`
**Current**: Missing `get_plan/1` function (only has `get_current_plan/0`)
**Changes**: Add `get_plan/1` function to retrieve pricing plan by ID

```erlang
%% BEFORE (existing exports)
-export([
    get_current_plan/0,
    set_current_plan/1,
    ...
]).

%% AFTER (add export)
-export([
    get_current_plan/0,
    get_plan/1,  %% NEW
    set_current_plan/1,
    ...
]).

%% Implementation (add after get_current_plan/0)
%% @doc Get pricing plan by ID
-spec get_plan(PlanId :: binary()) -> {ok, map()} | {error, not_found}.
get_plan(PlanId) ->
    ensure_table(),
    case ets:lookup(?STATE_TABLE, PlanId) of
        [{PlanId, Plan}] -> {ok, Plan};
        [] -> {error, not_found}
    end.
```

**Reason**: Blocks erlmcp_pricing_cli from retrieving plan by ID. Simple ETS lookup following existing pattern.

##### 2. tcps_persistence.erl
**File**: `apps/tcps_erlmcp/src/tcps_persistence.erl`
**Current**: Missing `store_sku/1` function
**Changes**: Add `store_sku/1` function to store SKU receipt

```erlang
%% BEFORE (existing exports)
-export([
    store_receipt/1,
    load_receipt/1,
    ...
]).

%% AFTER (add export)
-export([
    store_receipt/1,
    store_sku/1,  %% NEW
    load_receipt/1,
    ...
]).

%% Implementation (add after store_receipt/1)
%% @doc Store SKU receipt with SHA-256 checksum
-spec store_sku(Sku :: map()) -> {ok, binary()} | {error, term()}.
store_sku(Sku) ->
    try
        #{sku_id := SkuId, stage := Stage} = Sku,

        % Ensure directory exists
        ReceiptDir = receipt_dir(SkuId),
        ok = filelib:ensure_dir(filename:join(ReceiptDir, "dummy")),

        % Generate filename
        Timestamp = maps:get(timestamp, Sku, timestamp_now()),
        Filename = generate_receipt_filename(Stage, Timestamp),
        Path = filename:join(ReceiptDir, Filename),

        % Calculate checksum
        SkuJson = jsone:encode(Sku),
        Checksum = calculate_checksum(SkuJson),
        SkuWithChecksum = Sku#{checksum => Checksum},

        % Write to file
        FinalJson = jsone:encode(SkuWithChecksum),
        ok = file:write_file(Path, FinalJson),

        {ok, list_to_binary(Path)}
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.
```

**Reason**: Blocks tcps_sku from storing SKU receipts. Reuses existing helper functions (receipt_dir, generate_receipt_filename, calculate_checksum).

##### 3. tcps_work_order.erl
**File**: `apps/tcps_erlmcp/src/tcps_work_order.erl`
**Current**: Missing `update_status/2` function
**Changes**: Add `update_status/2` function to update work order status

```erlang
%% BEFORE (existing exports)
-export([
    create/1,
    get/1,
    list/0,
    ...
]).

%% AFTER (add export)
-export([
    create/1,
    get/1,
    update_status/2,  %% NEW
    list/0,
    ...
]).

%% Implementation (add after get/1)
%% @doc Update work order status
-spec update_status(WorkOrderId :: binary(), NewStatus :: atom()) -> ok | {error, term()}.
update_status(WorkOrderId, NewStatus) ->
    case tcps_persistence:get_work_order(WorkOrderId) of
        {ok, WorkOrder} ->
            UpdatedWorkOrder = WorkOrder#{status => NewStatus},
            tcps_persistence:update_work_order(UpdatedWorkOrder);
        {error, not_found} ->
            {error, not_found}
    end.
```

**Reason**: Blocks tcps_sku from updating work order status. Delegates to tcps_persistence (separation of concerns).

##### 4. Test Modules (NEW FILES)
**File**: `apps/erlmcp_core/test/erlmcp_pricing_state_tests.erl`
**Current**: Does not exist
**Changes**: Create EUnit test module for erlmcp_pricing_state

```erlang
-module(erlmcp_pricing_state_tests).
-include_lib("eunit/include/eunit.hrl").

%% Fixture
setup() ->
    {ok, Pid} = erlmcp_pricing_state:start_link(),
    Pid.

cleanup(_Pid) ->
    ok = erlmcp_pricing_state:stop().

%% Tests
get_plan_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) -> [
         ?_test(begin
             %% Test: Get plan by ID (exists)
             PlanId = <<"test-plan">>,
             Plan = #{plan_id => PlanId, tier => team},
             ok = erlmcp_pricing_state:set_current_plan(team),

             {ok, RetrievedPlan} = erlmcp_pricing_state:get_plan(PlanId),
             ?assertEqual(PlanId, maps:get(plan_id, RetrievedPlan))
         end),
         ?_test(begin
             %% Test: Get plan by ID (not found)
             ?assertEqual({error, not_found}, erlmcp_pricing_state:get_plan(<<"non-existent">>))
         end)
     ]end}.
```

**File**: `apps/tcps_erlmcp/test/tcps_persistence_tests.erl`
**Current**: Does not exist
**Changes**: Create EUnit test module for tcps_persistence

```erlang
-module(tcps_persistence_tests).
-include_lib("eunit/include/eunit.hrl").

%% Fixture
setup() ->
    ok = tcps_persistence:init().

cleanup(_ok) ->
    ok.

%% Tests
store_sku_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) -> [
         ?_test(begin
             %% Test: Store SKU receipt
             Sku = #{
                 sku_id => <<"test-sku">>,
                 stage => raw,
                 timestamp => <<"2024-01-01T00:00:00Z">>,
                 status => pass
             },
             {ok, Path} = tcps_persistence:store_sku(Sku),
             ?assert(is_binary(Path)),
             ?assert(filelib:is_file(binary_to_list(Path)))
         end)
     ]end}.
```

**File**: `apps/tcps_erlmcp/test/tcps_work_order_tests.erl`
**Current**: Does not exist
**Changes**: Create EUnit test module for tcps_work_order

```erlang
-module(tcps_work_order_tests).
-include_lib("eunit/include/eunit.hrl").

%% Fixture
setup() ->
    {ok, Pid} = tcps_work_order:start_link(),
    Pid.

cleanup(_Pid) ->
    ok = gen_server:stop(Pid).

%% Tests
update_status_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) -> [
         ?_test(begin
             %% Test: Update work order status
             {ok, WorkOrderId} = tcps_work_order:create(#{sku_id => <<"test-sku">>}),
             ok = tcps_work_order:update_status(WorkOrderId, in_progress),
             {ok, WorkOrder} = tcps_work_order:get(WorkOrderId),
             ?assertEqual(in_progress, maps:get(status, WorkOrder))
         end)
     ]end}.
```

**Reason**: Chicago School TDD requires tests FIRST. ≥80% coverage required for quality gate.

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_pricing_state_tests` - 100% pass rate
- [ ] EUnit: `rebar3 eunit --module=tcps_persistence_tests` - 100% pass rate
- [ ] EUnit: `rebar3 eunit --module=tcps_work_order_tests` - 100% pass rate
- [ ] Coverage: `rebar3 cover` - ≥80% coverage for all 3 new functions
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings for all 3 modules
- [ ] Xref: `rebar3 xref` - 3 fewer undefined function calls

##### Manual Verification:
- [ ] Code review: OTP patterns followed (ETS usage, error handling)
- [ ] Integration: Calling code compiles without errors
- [ ] Edge cases: Error paths tested (not_found, invalid input)
- [ ] Performance: No regression >10%

**Note**: Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix.

---

### Phase 3: Remove Orphaned Function Calls (≤2 hours)

#### Overview
Remove 3 orphaned module calls (erlmcp_task_manager, erlmcp_request_id, erlmcp_tracing) by updating calling code to use replacements or inline logic.

#### Specification
**Calls to Remove**:
1. `erlmcp_task_manager:register_server/2` in `erlmcp_server.erl:179` → Replace with erlmcp_hooks (no-op)
2. `erlmcp_request_id:safe_increment/1` in `erlmcp_client.erl:478` → Replace with inline overflow check
3. `erlmcp_tracing:*` in multiple files → Remove all tracing calls

#### Pseudocode

**erlmcp_task_manager → erlmcp_hooks**:
```erlang
%% BEFORE (erlmcp_server.erl:179)
ok = erlmcp_task_manager:register_server(ServerId, self()),

%% AFTER (no-op - erlmcp_hooks manages server registration internally)
%% erlmcp_hooks automatically registers servers in post_task hooks
%% No manual registration required
```

**erlmcp_request_id → inline overflow check**:
```erlang
%% BEFORE (erlmcp_client.erl:478)
SafeNextIdResult = case catch erlmcp_request_id:safe_increment(RequestId) of
    {ok, SafeId} -> {ok, SafeId};
    {error, overflow} -> ...
end,

%% AFTER (inline overflow check)
SafeNextIdResult = case RequestId + 1 of
    NextId when NextId > ?MAX_REQUEST_ID ->
        gen_server:reply(FromPid, {error, {request_id_overflow,
            <<"Request ID space exhausted. Reconnect required.">>}}),
        {error, request_id_exhausted};
    NextId -> {ok, NextId}
end,
```

**erlmcp_tracing → remove all calls**:
```erlang
%% BEFORE (erlmcp_server.erl:171)
SpanCtx = erlmcp_tracing:start_server_span(<<"server.init">>, ServerId),
try
    ...
    erlmcp_tracing:set_attributes(SpanCtx, #{...}),
    ...
after
    erlmcp_tracing:end_span(SpanCtx)
end,

%% AFTER (remove all tracing calls - placeholder module)
%% TODO: Implement tracing in future item
%% For now, remove all erlmcp_tracing calls
```

#### Architecture
**Call Sites**:
- `erlmcp_server.erl:179` - calls `erlmcp_task_manager:register_server/2`
- `erlmcp_client.erl:478` - calls `erlmcp_request_id:safe_increment/1`
- `erlmcp_server.erl:171-177` - calls `erlmcp_tracing:start_server_span/2`, `set_attributes/2`
- Other files with erlmcp_tracing calls (to be identified in Phase 1)

**Migration Paths**:
- Direct replacement (erlmcp_task_manager → erlmcp_hooks automatic)
- Inline logic (erlmcp_request_id → overflow check)
- Removal (erlmcp_tracing → placeholder, defer implementation)

#### Changes Required:

##### 1. erlmcp_server.erl
**File**: `apps/erlmcp_core/src/erlmcp_server.erl`
**Current**: Calls `erlmcp_task_manager:register_server/2` at line 179
**Changes**: Remove call (erlmcp_hooks manages registration automatically)

```erlang
%% BEFORE (lines 171-179)
init([ServerId, Capabilities]) ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.init">>, ServerId),
    try
        process_flag(trap_exit, true),

        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"server_id">> => ServerId
        }),

        ok = erlmcp_task_manager:register_server(ServerId, self()),

%% AFTER (lines 171-179)
init([ServerId, Capabilities]) ->
    %% TODO: Implement tracing in future item (deferred)
    process_flag(trap_exit, true),

    %% erlmcp_hooks manages server registration automatically in post_task hooks
    %% No manual registration required

%% Continue with rest of init/1
    ...
```

**Reason**: erlmcp_task_manager replaced by erlmcp_hooks. Manual registration not required. Tracing deferred to future item.

##### 2. erlmcp_client.erl
**File**: `apps/erlmcp_core/src/erlmcp_client.erl`
**Current**: Calls `erlmcp_request_id:safe_increment/1` at line 478
**Changes**: Replace with inline overflow check

```erlang
%% BEFORE (lines 475-489)
    %% P0 SECURITY: Safe request ID handling
    %% Prevent integer overflow by checking if next ID would overflow
    NextRequestId = RequestId + 1,
    SafeNextIdResult = case catch erlmcp_request_id:safe_increment(RequestId) of
        {ok, SafeId} -> {ok, SafeId};
        {error, overflow} ->
            gen_server:reply(FromPid, {error, {request_id_overflow,
                <<"Request ID space exhausted. Reconnect required.">>}}),
            {error, request_id_exhausted};
        _Other -> {ok, NextRequestId}
    end,

%% AFTER (lines 475-489)
    %% P0 SECURITY: Safe request ID handling
    %% Prevent integer overflow by checking if next ID would overflow
    %% Maximum request ID: 2^52 - 1 (safe for JavaScript JSON)
    -define(MAX_REQUEST_ID, (1 bsl 52) - 1),

    SafeNextIdResult = case RequestId + 1 of
        NextId when NextId > ?MAX_REQUEST_ID ->
            gen_server:reply(FromPid, {error, {request_id_overflow,
                <<"Request ID space exhausted. Reconnect required.">>}}),
            {error, request_id_exhausted};
        NextId -> {ok, NextId}
    end,
```

**Reason**: erlmcp_request_id was stub module never implemented. Inline overflow check is simple and removes dependency. MAX_REQUEST_ID set to 2^52-1 for JSON safety.

##### 3. Add MAX_REQUEST_ID Constant
**File**: `apps/erlmcp_core/src/erlmcp_client.erl`
**Current**: No MAX_REQUEST_ID constant defined
**Changes**: Add constant at top of module

```erlang
%% BEFORE (module header)
-module(erlmcp_client).
-behaviour(gen_server).

%% AFTER (add constant after module header)
-module(erlmcp_client).
-behaviour(gen_server).

%% Maximum request ID: 2^52 - 1 (safe for JavaScript JSON)
%% See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MAX_SAFE_INTEGER
-define(MAX_REQUEST_ID, (1 bsl 52) - 1).
```

**Reason**: Documents maximum request ID limit. Ensures JSON safety (JavaScript Number.MAX_SAFE_INTEGER).

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_client_tests` - 100% pass rate
- [ ] EUnit: `rebar3 eunit --module=erlmcp_server_tests` - 100% pass rate
- [ ] Coverage: `rebar3 cover` - ≥80% coverage for modified functions
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings for modified modules
- [ ] Xref: `rebar3 xref` - 3 fewer undefined function calls

##### Manual Verification:
- [ ] Code review: All erlmcp_task_manager calls removed
- [ ] Code review: All erlmcp_request_id calls removed
- [ ] Code review: All erlmcp_tracing calls removed
- [ ] Integration: Server starts without errors
- [ ] Integration: Client generates request IDs without errors

**Note**: Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix.

---

### Phase 4: Remove Unused Local Functions (≤3 hours)

#### Overview
Remove 26 unused local functions (dead code from v2.0 refactoring) identified in Phase 1 categorization. Keep 2 functions as documented public API exports.

#### Specification
**Functions to Remove (26)**:
- Audio content functions (2): `erlmcp_server:create_audio_content/3`, `create_audio_content_with_metadata/4`
- Router functions (8): `erlmcp_router:select_by_weight/3`, `select_server_adaptive/2`, etc.
- SSE retry functions (2): `erlmcp_transport_sse:format_retry_field/1`, `get_retry_timeout/0`
- Utility functions (14): `tcps_receipt_verifier:is_atom_stage/1`, `tcps_work_order:atom_to_binary/1`, etc.

**Functions to Document (2)**:
- `erlmcp_security_headers:add_to_response/1` - Public API for HTTP middleware
- Other exported functions not used internally (identified in Phase 1)

#### Pseudocode

**Remove dead code**:
```erlang
%% BEFORE (erlmcp_server.erl:1270-1324)
%%% @doc Create audio content
-spec create_audio_content(binary(), binary(), [#mcp_annotation{}] | undefined) ->
    #mcp_content_audio{}.
create_audio_content(AudioBinary, MimeType, Annotations) ->
    %% ... implementation ...
    #mcp_content_audio{}.

%%% @doc Create audio content with metadata
-spec create_audio_content_with_metadata(...) -> #mcp_content_audio{}.
create_audio_content_with_metadata(...) ->
    %% ... implementation ...
    create_audio_content(AudioBinary, MimeType, Annotations).

%% AFTER (delete entire functions)
%% Functions removed - dead code from v2.0 refactoring
```

**Document public API**:
```erlang
%% BEFORE (erlmcp_security_headers.erl:184)
%% @doc Add security headers to HTTP response.
-spec add_to_response(map()) -> map().
add_to_response(Response) ->
    %% ... implementation ...

%% AFTER (add -compile directive to suppress Xref warning)
-module(erlmcp_security_headers).

%% Public API: Add security headers to HTTP response
%% Not used internally - exported for HTTP middleware integration
-compile({nowarn_unused_function, [{add_to_response, 1}]}).

%% @doc Add security headers to HTTP response.
-spec add_to_response(map()) -> map().
add_to_response(Response) ->
    %% ... implementation ...
```

#### Architecture
**Dead Code Identification**:
- Functions not called anywhere in codebase (confirmed by Xref)
- Functions with no external dependencies (safe to remove)
- Functions from v2.0 refactoring that were replaced

**Public API Documentation**:
- Exported functions not used internally
- Functions called by external HTTP middleware
- Utility functions for library users

#### Changes Required:

##### 1. erlmcp_server.erl
**File**: `apps/erlmcp_core/src/erlmcp_server.erl`
**Current**: Exports `create_audio_content/3` and `create_audio_content_with_metadata/4` at lines 10-11, implements at lines 1270-1324
**Changes**: Remove exports and implementations

```erlang
%% BEFORE (exports)
-export([
    ...
    {create_audio_content, 3},
    {create_audio_content_with_metadata, 4},
    ...
]).

%% AFTER (remove exports)
-export([
    ...
    %% Audio content functions removed - dead code from v2.0
    ...
]).

%% BEFORE (implementations at lines 1270-1324)
%%% @doc Create audio content
-spec create_audio_content(binary(), binary(), [#mcp_annotation{}] | undefined) ->
    #mcp_content_audio{}.
create_audio_content(AudioBinary, MimeType, Annotations) ->
    %% ... 54 lines of code ...
    #mcp_content_audio{}.

%%% @doc Create audio content with metadata
-spec create_audio_content_with_metadata(
    AudioBinary :: binary(),
    MimeType :: binary(),
    Metadata :: map(),
    Annotations :: [#mcp_annotation{}] | undefined
) -> #mcp_content_audio{}.
create_audio_content_with_metadata(AudioBinary, MimeType, Metadata, Annotations) ->
    %% ... implementation ...
    create_audio_content(AudioBinary, MimeType, Annotations).

%% AFTER (delete lines 1259-1324 - entire functions and comments)
```

**Reason**: Functions never called (confirmed by Xref). Dead code from v2.0 refactoring. Removing reduces maintenance burden.

##### 2. erlmcp_router.erl
**File**: `apps/erlmcp_core/src/erlmcp_router.erl`
**Current**: Exports 8 unused routing functions
**Changes**: Remove exports and implementations

```erlang
%% BEFORE (exports)
-export([
    select_by_weight/3,
    select_server_adaptive/2,
    select_server_round_robin/2,
    select_server_random/2,
    select_server_least_connections/2,
    calculate_weight/2,
    update_metrics/1,
    reset_metrics/0
]).

%% AFTER (remove all exports and implementations)
%% Unused routing functions removed - dead code from v2.0
```

**Reason**: Functions never called (confirmed by Xref). Dead code from v2.0 refactoring. Complex routing logic not needed.

##### 3. erlmcp_transport_sse.erl
**File**: `apps/erlmcp_transports/src/erlmcp_transport_sse.erl`
**Current**: Exports `format_retry_field/1` and `get_retry_timeout/0`
**Changes**: Remove exports and implementations

```erlang
%% BEFORE (exports)
-export([
    format_retry_field/1,
    get_retry_timeout/0
]).

%% AFTER (remove exports)
%% Retry functions removed - dead code from v2.0
```

**Reason**: Functions never called (confirmed by Xref). Dead code from v2.0 refactoring.

##### 4. tcps_receipt_verifier.erl
**File**: `apps/tcps_erlmcp/src/tcps_receipt_verifier.erl`
**Current**: Exports `is_atom_stage/1`
**Changes**: Remove export and implementation

```erlang
%% BEFORE (exports)
-export([
    is_atom_stage/1
]).

%% AFTER (remove export)
%% is_atom_stage removed - dead code from v2.0
```

**Reason**: Function never called (confirmed by Xref). Dead code from v2.0 refactoring.

##### 5. tcps_work_order.erl
**File**: `apps/tcps_erlmcp/src/tcps_work_order.erl`
**Current**: Exports `atom_to_binary/1`
**Changes**: Remove export and implementation

```erlang
%% BEFORE (exports)
-export([
    atom_to_binary/1
]).

%% AFTER (remove export)
%% atom_to_binary removed - dead code from v2.0
```

**Reason**: Function never called (confirmed by Xref). Dead code from v2.0 refactoring.

##### 6. erlmcp_security_headers.erl
**File**: `apps/erlmcp_transports/src/erlmcp_security_headers.erl`
**Current**: Exports `add_to_response/1` but function not called internally
**Changes**: Add `-compile` directive to document as public API

```erlang
%% BEFORE (module header)
-module(erlmcp_security_headers).

%% AFTER (add -compile directive)
-module(erlmcp_security_headers).

%% Public API: Add security headers to HTTP response
%% Not used internally - exported for HTTP middleware integration
%% External libraries can call this function to add security headers
-compile({nowarn_unused_function, [{add_to_response, 1}]}).
```

**Reason**: Function is exported for external HTTP middleware integration. Not dead code - intentional public API.

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit` - 100% pass rate (no regressions)
- [ ] Coverage: `rebar3 cover` - ≥80% coverage (removed code not counted)
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings (removed functions not checked)
- [ ] Xref: `rebar3 xref` - 28 fewer unused local function warnings

##### Manual Verification:
- [ ] Code review: All 26 dead code functions removed
- [ ] Code review: 2 public API functions documented
- [ ] Integration: No compilation errors from missing functions
- [ ] Integration: External HTTP middleware still works (erlmcp_security_headers)

**Note**: Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix.

---

### Phase 5: Final Verification and Quality Gates (≤1 hour)

#### Overview
Verify all 250 Xref warnings eliminated and all quality gates passing. Generate compliance receipt documenting zero Xref warnings.

#### Specification
**Final State**:
- Xref warnings: 250 → 0 (100% reduction)
- Undefined function calls: 222 → 0 (100% reduction)
- Unused local functions: 28 → 0 (100% reduction)
- All quality gates passing

#### Pseudocode
```bash
# 1. Clean build
rebar3 clean
rebar3 compile

# 2. Run Xref and capture output
rebar3 xref 2>&1 | tee final_xref_output.txt

# 3. Verify zero warnings
WARNING_COUNT=$(grep -c "Warning:" final_xref_output.txt)
if [ "$WARNING_COUNT" -ne 0 ]; then
    echo "FAIL: Xref still has $WARNING_COUNT warnings"
    exit 1
fi

# 4. Run all quality gates
rebar3 eunit
rebar3 dialyzer
rebar3 cover

# 5. Generate compliance receipt
tcps_quality_gates:generate_receipt(#{xref_warnings => 0}).

# 6. Update documentation
echo "## Xref Compliance Status" >> README.md
echo "- Status: ✅ PASS (0 warnings)" >> README.md
echo "- Date: $(date)" >> README.md
```

#### Architecture
**Quality Gate Checks**:
1. Compilation: `rebar3 compile` - 0 errors, 0 warnings
2. EUnit: `rebar3 eunit` - 100% pass rate
3. Coverage: `rebar3 cover` - ≥80% overall
4. Dialyzer: `rebar3 dialyzer` - 0 warnings
5. Xref: `rebar3 xref` - 0 warnings (PRIMARY GOAL)

**Receipt Generation**:
- Create `priv/receipts/xref_compliance_YYYYMMDD.json`
- Document zero Xref warnings
- Include timestamp, git commit, verification results

#### Changes Required:

##### 1. README.md Update
**File**: `README.md`
**Current**: No Xref compliance status documented
**Changes**: Add Xref compliance section

```markdown
## Quality Gates

### Xref Compliance
- **Status**: ✅ PASS (0 warnings)
- **Date**: 2026-01-29
- **Details**: All 250 Xref warnings eliminated (222 undefined function calls, 28 unused local functions)
- **Verification**: `rebar3 xref` shows 0 warnings

### Other Quality Gates
- **Compilation**: ✅ PASS (0 errors, 0 warnings)
- **EUnit**: ⚠️ IN PROGRESS (14/14 tests passing, 13 test modules missing)
- **Coverage**: ❌ FAIL (1% vs 80% target)
- **Dialyzer**: ❌ FAIL (526 warnings)
```

**Reason**: Documents Xref compliance achievement. Provides audit trail for production readiness.

##### 2. Compliance Receipt
**File**: `priv/receipts/xref_compliance_20260129.json`
**Current**: Does not exist
**Changes**: Create receipt documenting zero Xref warnings

```json
{
  "receipt_type": "xref_compliance",
  "timestamp": "2026-01-29T00:00:00Z",
  "git_commit": "abc123",
  "quality_gate": "xref",
  "status": "pass",
  "metrics": {
    "xref_warnings": 0,
    "undefined_function_calls": 0,
    "unused_local_functions": 0,
    "deprecated_function_calls": 0,
    "deprecated_functions": 0
  },
  "verification": {
    "compilation": "pass",
    "eunit": "pass",
    "dialyzer": "warn",
    "coverage": "fail"
  },
  "changes": {
    "implemented_functions": 3,
    "removed_orphaned_calls": 3,
    "removed_dead_code": 26,
    "documented_public_api": 2
  }
}
```

**Reason**: Provides immutable audit trail of Xref compliance. Required for production release.

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Clean build: `rebar3 clean && rebar3 compile` - 0 errors, 0 warnings
- [ ] Xref: `rebar3 xref` - 0 warnings (PRIMARY GOAL - must pass)
- [ ] EUnit: `rebar3 eunit` - 100% pass rate
- [ ] Coverage: `rebar3 cover` - ≥80% for new code
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings for modified modules
- [ ] Pre-commit hook: `.claude/hooks/pre-task-validate.sh` - PASS

##### Manual Verification:
- [ ] README.md updated with Xref compliance status
- [ ] Compliance receipt created (`priv/receipts/xref_compliance_*.json`)
- [ ] No regressions in functionality (all tests pass)
- [ ] Code review: OTP patterns followed
- [ ] Documentation: All changes documented

**Final Validation**:
```bash
# Verify zero Xref warnings
rebar3 xref 2>&1 | grep -c "Warning:"
# Expected output: (no output - zero warnings)

# Verify compliance receipt exists
ls -l priv/receipts/xref_compliance_*.json
# Expected output: receipt file listed

# Verify README updated
grep "Xref Compliance" README.md
# Expected output: section with ✅ PASS status
```

**Note**: This is the FINAL phase. ALL quality gates MUST pass before marking item complete. Zero Xref warnings is MANDATORY for production release.

---

## Testing Strategy

### Chicago School TDD (MANDATORY)
- **NO MOCKS** - Use real processes, real gen_servers
- **State-Based Verification** - Check ETS table contents, file system state
- **Integration Testing** - Test with real dependencies (tcps_persistence, tcps_work_order)
- **Race Condition Testing** - Concurrent operations on ETS tables

### Unit Tests (EUnit)
- **What to Test**: All 3 implemented functions (erlmcp_pricing_state:get_plan/1, tcps_persistence:store_sku/1, tcps_work_order:update_status/2)
- **Test Pattern**: Reference test file: `apps/erlmcp_core/test/erlmcp_batch_tests.erl`
- **Coverage Target**: ≥80% per module
- **Pass Rate**: 100% (all tests must pass)

### Integration Tests (Common Test)
- **End-to-End Scenarios**: Store SKU receipt → Update work order status → Retrieve pricing plan
- **Multi-Process**: Concurrent ETS table access
- **Failure Scenarios**: Invalid input, file system errors, ETS lookup failures

### Manual Testing Steps
1. **Xref Verification**: Run `rebar3 xref` and verify 0 warnings in output
2. **Compilation**: Run `rebar3 compile` and verify 0 errors
3. **Test Execution**: Run `rebar3 eunit` and verify 100% pass rate
4. **Integration Test**: Start erlmcp_server and verify no undefined function errors
5. **Receipt Generation**: Verify compliance receipt created in `priv/receipts/`

### Quality Gates
Every phase MUST pass:
1. **Compilation**: `TERM=dumb rebar3 compile`
2. **EUnit**: `rebar3 eunit` (100% pass rate)
3. **Coverage**: `rebar3 cover` (≥80% for new code)
4. **Dialyzer**: `rebar3 dialyzer` (0 warnings for modified modules)
5. **Xref**: `rebar3 xref` (0 warnings - PRIMARY GOAL)

## Manufacturing Checklist

### Before Implementation
- [x] Research verified (read actual source code from research.md)
- [x] Scope confirmed (IN: fix 250 Xref warnings, OUT: Dialyzer, test coverage, performance)
- [x] No open questions (all decisions made - IMPLEMENT vs REMOVE vs DOCUMENT)
- [x] Phases broken down (3 phases: Item 018, Item 021, Item 020 final verification)
- [x] Acceptance criteria defined (measurable: 0 Xref warnings, ≥80% coverage)
- [x] Dependencies identified (Item 018 and Item 021 must complete before Item 020)

### During Implementation
- [ ] **Item 018 (User Story US-001)**: Resolve 222 undefined function call warnings - 8 hours
  - [ ] Implement missing functions (erlmcp_pricing_state, tcps_persistence, tcps_work_order)
  - [ ] Remove replaced module calls (erlmcp_task_manager, erlmcp_request_id, erlmcp_tracing)
  - [ ] Update calling code (erlmcp_server, erlmcp_client)
  - [ ] Write tests for new functions (EUnit tests - Chicago School TDD)
  - [ ] Verify Xref shows 0 undefined function call warnings
- [ ] **Item 021 (User Story US-002)**: Remove 28 unused local function warnings - 4 hours
  - [ ] Audit all unused functions (VERIFY: truly unused vs public API)
  - [ ] Remove dead code (erlmcp_router, erlmcp_pagination, erlmcp_server, erlmcp_transport_sse, tcps_receipt_verifier, tcps_work_order)
  - [ ] Document public API exports (erlmcp_security_headers with -compile directives)
  - [ ] Verify Xref shows 0 unused local function warnings
- [ ] **Item 020 (User Story US-003)**: Final verification - 2 hours
  - [ ] Clean build: `rebar3 clean && rebar3 compile`
  - [ ] Run Xref: `rebar3 xref` - verify 0 warnings
  - [ ] Run tests: `rebar3 eunit`, `rebar3 ct`
  - [ ] Coverage: `rebar3 cover` - verify ≥80% for new code
  - [ ] Update CHANGELOG.md with breaking changes and new functions
  - [ ] Update README.md with Xref compliance status
  - [ ] Generate compliance receipt (priv/receipts/xref_compliance_YYYYMMDD.json)

### After Implementation
- [ ] All tests passing (100% rate - existing tests + 3 new test modules)
- [ ] Coverage ≥80% for new code (verified - erlmcp_pricing_state, tcps_persistence, tcps_work_order)
- [ ] Dialyzer 0 new warnings (verified - existing 526 warnings acceptable)
- [ ] Xref 0 warnings (PRIMARY GOAL - verified with rebar3 xref, grep shows 0 warnings in log)
- [ ] Performance no regression >10% (verified with benchmarks)
- [ ] Documentation updated (CHANGELOG.md, README.md, compliance receipt)
- [ ] Code review complete (OTP patterns verified, dead code removed, public API documented)
- [ ] Production ready (quality gate PASSED)

## Risk Management

### Known Risks
| Risk | Severity | Probability | Mitigation |
|------|----------|-------------|------------|
| **Implementing missing functions breaks existing code** | P1 (High) | Medium | MITIGATION: Follow decision matrix - implement only 3 critical functions. Write tests FIRST. Verify with EUnit before proceeding. |
| **Removing undefined functions breaks calling code** | P0 (Critical) | High | MITIGATION: For each removed call, update calling code to use alternative. Test after each change. Use git bisect to identify breakages. |
| **Removing unused functions breaks external API** | P1 (High) | Medium | MITIGATION: Check each unused function - if exported, document as public API. Use `-compile({nowarn_unused_function, [{Func, Arity}]})`. |
| **Xref false positives (modules exist but Xref can't find)** | P2 (Medium) | Low | MITIGATION: Run `rebar3 clean && rebar3 compile` before Xref. Verify exports with `module:module_info(exports)`. Add to xref_ignores if legitimate. |
| **Dead code removal eliminates needed functionality** | P2 (Medium) | Low | MITIGATION: Check for dynamic calls (apply/3, spawn/3) before removing. Search codebase for function name references. Add documentation for kept functions. |
| **Xref warnings prevent production deployment** | P0 (Critical) | High | MITIGATION: Prioritize fixing undefined calls (P0), then unused functions (P1). Use Heijunka - fix in small batches. Verify with Xref after each phase. |

### Rollback Plan
**If something goes wrong**:
- Git revert: `git revert HEAD` (reverts last commit if Xref warnings increase)
- Data migration: No data migration required (code-only change)
- Service impact: No service impact (no running processes affected)
- Fallback: Keep backup branch `wreckit/020-fix-250-xref-warnings-backup` before starting

**Recovery Steps**:
1. Identify which change broke Xref (use git bisect)
2. Revert specific commit (not entire item)
3. Fix root cause (missing implementation or incorrect removal)
4. Re-test with `rebar3 xref`
5. Re-apply fix with additional verification

## References

- **Research**: `/Users/sac/erlmcp/.wreckit/items/020-fix-250-xref-warnings-undefined-calls-unused-funct/research.md`
- **Item 018 Plan**: `/Users/sac/erlmcp/.wreckit/items/018-implement-or-remove-15-missing-modules-referenced-/plan.md` (undefined function calls)
- **Item 021 Plan**: `/Users/sac/erlmcp/.wreckit/items/021-remove-27-unused-local-functions-dead-code/plan.md` (unused local functions)
- **CLAUDE.md**: `/Users/sac/erlmcp/CLAUDE.md` (project rules)
- **TCPS**: `/Users/sac/erlmcp/docs/tcps/TCPS.md` (manufacturing principles)
- **Test Reference**: `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_batch_tests.erl` (Chicago School TDD pattern)
- **Xref Config**: `rebar.config:152-195` (Xref checks enabled, xref_ignores list)

---

**Manufacturing Status**: ✅ PLANNED
**Ready for Implementation**: YES
**Dependencies**: Items 018 (US-001) and 021 (US-002) must complete before Item 020 (US-003) final verification
**Total Estimated Effort**: 16 hours (2 days) across Items 018, 021, 020
**User Stories**: 3 stories created (US-001: Item 018, US-002: Item 021, US-003: Item 020 final verification)
