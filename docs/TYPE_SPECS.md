# Type Specification Best Practices Guide

**Project**: erlmcp - Model Context Protocol (MCP) for Erlang
**Focus**: Toyota Code Production System (TCPS) Type Safety

---

## Table of Contents

1. [When to Add Specs](#when-to-add-specs)
2. [How to Write Good Specs](#how-to-write-good-specs)
3. [Common Type Patterns](#common-type-patterns)
4. [TCPS-Specific Types](#tcps-specific-types)
5. [Testing Specs with PropEr](#testing-specs-with-proper)
6. [Dialyzer Configuration](#dialyzer-configuration)
7. [Common Pitfalls](#common-pitfalls)
8. [Examples from TCPS Modules](#examples-from-tcps-modules)

---

## When to Add Specs

### ALWAYS Add Specs For

1. **All Exported Functions** (100% coverage goal)
   ```erlang
   -export([create_work_order/1, get_status/1]).

   -spec create_work_order(pull_signal()) -> {ok, work_order_id()} | {error, term()}.
   -spec get_status(work_order_id()) -> {ok, map()} | {error, not_found}.
   ```

2. **gen_server/gen_statem Callbacks**
   ```erlang
   -spec init(Args :: list()) -> {ok, state()}.
   -spec handle_call(Request :: term(), From :: {pid(), term()}, State :: state()) ->
       {reply, Reply :: term(), NewState :: state()}.
   ```

3. **Complex Internal Functions** (>20 lines or non-obvious logic)
   ```erlang
   -spec apply_heijunka_leveling([work_order()], pos_integer()) -> leveled_schedule().
   ```

4. **Functions with Non-Obvious Types**
   ```erlang
   %% Not obvious what this returns
   -spec calculate_deadline(Hours :: pos_integer() | infinity) -> erlang:timestamp().
   ```

### OPTIONAL But Recommended

1. **Simple getter/setter functions** (if types are obvious)
2. **Pure helper functions** with self-documenting names
3. **Private functions** that are well-tested

---

## How to Write Good Specs

### 1. Use Custom Types for Clarity

**Bad** (overly generic):
```erlang
-spec process_data(binary(), map()) -> map().
```

**Good** (specific and clear):
```erlang
-type work_order_id() :: binary().
-type bucket() :: reliability | security | cost | compliance.
-type work_order() :: #{
    id := work_order_id(),
    bucket := bucket(),
    priority := 1..10,
    status := pending | in_progress | completed
}.

-spec create_work_order(pull_signal()) ->
    {ok, work_order_id()} | {error, limit_reached | invalid_signal}.
```

### 2. Specify Map Keys Where Possible

**Bad** (loses all type information):
```erlang
-spec build_receipt(map()) -> map().
```

**Good** (clear input/output contracts):
```erlang
-spec build_receipt(#{
    sku_id := binary(),
    stage := stage(),
    status := atom()
}) -> #{
    receipt_id := binary(),
    timestamp := integer(),
    sku_id := binary(),
    stage := atom()
}.
```

### 3. Document Error Reasons

**Bad** (unhelpful `term()`):
```erlang
-spec process_signal(signal()) -> {ok, result()} | {error, term()}.
```

**Good** (specific error types):
```erlang
-type error_reason() :: limit_reached
                      | invalid_bucket
                      | not_found
                      | {dependency_failed, work_order_id()}.

-spec process_pull_signal(pull_signal()) ->
    {ok, work_order_id()} | {error, error_reason()}.
```

### 4. Use Guards for Numeric Ranges

**Bad** (too permissive):
```erlang
-spec set_priority(integer()) -> ok.
```

**Good** (constrained range):
```erlang
-spec set_priority(Priority :: 1..10) -> ok | {error, out_of_range}.
```

### 5. Leverage `when` Clauses for Complex Types

```erlang
-spec process_batch(Requests) -> Results
    when Requests :: [work_order()],
         Results :: [{ok, work_order_id()} | {error, error_reason()}].
```

---

## Common Type Patterns

### 1. OK/Error Tuples

```erlang
%% Simple success/failure
-spec function() -> ok | {error, Reason}
    when Reason :: atom() | {atom(), term()}.

%% With success value
-spec function() -> {ok, Result} | {error, Reason}
    when Result :: term(),
         Reason :: atom().

%% Multiple error types
-type create_error() :: limit_reached | invalid_input | {db_error, term()}.
-spec create_item(Data :: map()) -> {ok, item_id()} | {error, create_error()}.
```

### 2. Lists of Specific Types

```erlang
%% List of work orders
-spec get_work_orders(bucket()) -> [work_order()].

%% Non-empty list
-spec process_at_least_one([work_order(), ...]) -> ok.

%% List with specific length
-spec get_top_five() -> [work_order()] when length([work_order()]) =:= 5.
```

### 3. Maps with Known Keys

```erlang
%% Required keys only
-type andon_context() :: #{
    sku_id := binary(),
    stage := stage(),
    details := map()
}.

%% Required and optional keys
-type work_order() :: #{
    id := work_order_id(),
    bucket := bucket(),
    priority := priority(),
    metadata => map(),      % Optional
    sku_id => sku_id()      % Optional
}.
```

### 4. Union Types (Sum Types)

```erlang
-type failure_type() :: shacl_violation
                      | test_failure
                      | non_determinism
                      | missing_receipt
                      | compilation_failure.

-type stage() :: compilation
               | testing
               | validation
               | execution
               | integration
               | deployment.
```

### 5. Opaque Types (Information Hiding)

```erlang
%% In module definition
-opaque internal_state() :: #{
    counter := non_neg_integer(),
    data := term()
}.
-export_type([internal_state/0]).

%% Users can't pattern match internals, only pass around
-spec get_state() -> internal_state().
-spec update_state(internal_state(), term()) -> internal_state().
```

### 6. Recursive Types

```erlang
%% Tree structure
-type tree(T) :: {node, T, tree(T), tree(T)} | {leaf, T} | empty.

%% Nested work order dependencies
-type dependency_tree() :: #{
    work_order_id() => [dependency_tree()]
}.
```

---

## TCPS-Specific Types

### Kanban System Types

```erlang
-type bucket() :: reliability | security | cost | compliance.
-type work_order_id() :: binary().

-type work_order() :: #{
    id := work_order_id(),
    bucket := bucket(),
    priority := non_neg_integer(),
    created_at := erlang:timestamp(),
    status := pending | in_progress | completed,
    payload := map()
}.

-type pull_signal() :: #{
    bucket := bucket(),
    priority => non_neg_integer(),
    payload := map()
}.

-type leveled_schedule() :: [{bucket(), [work_order()]}].

-type wip_status() :: #{
    current := non_neg_integer(),
    limit := non_neg_integer() | infinity,
    available := non_neg_integer() | infinity,
    utilization := float()
}.
```

### Andon System Types

```erlang
-type failure_type() :: shacl_violation
                      | test_failure
                      | non_determinism
                      | missing_receipt
                      | compilation_failure.

-type stage() :: compilation
               | testing
               | validation
               | execution
               | integration
               | deployment.

-type andon_event_id() :: binary().
-type sku_id() :: binary().

-type andon_context() :: #{
    sku_id := sku_id(),
    stage := stage(),
    details := map(),
    metadata => map()
}.

-type andon_event() :: #{
    event_id := andon_event_id(),
    failure_type := failure_type(),
    sku_id := sku_id(),
    stage := stage(),
    timestamp := integer(),
    details := map(),
    status := open | resolved,
    resolution => resolution(),
    metadata => map()
}.

-type resolution() :: #{
    root_cause := binary(),
    fix_applied := binary(),
    prevention_added := binary(),
    resolver => binary(),
    resolution_time_minutes => pos_integer(),
    resolution_timestamp := integer()
}.
```

### Receipt Types

```erlang
-type receipt() :: #{
    receipt_id := binary(),
    andon_event_id => andon_event_id(),
    timestamp := integer(),
    timestamp_iso := binary(),
    sku_id := sku_id(),
    stage := stage(),
    status := binary(),
    receipt_type := andon_event | resolution | work_order,
    ontology_refs := [binary()],
    details => map()
}.
```

---

## Testing Specs with PropEr

### Basic Property Test

```erlang
-module(tcps_kanban_prop_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Generators
bucket() ->
    oneof([reliability, security, cost, compliance]).

priority() ->
    range(1, 10).

work_order() ->
    ?LET({Bucket, Priority},
         {bucket(), priority()},
         #{bucket => Bucket,
           priority => Priority,
           payload => #{}}).

%% Property: WIP limit is never exceeded
prop_wip_limit_respected() ->
    ?FORALL({Limit, Orders},
            {range(1, 10), list(work_order())},
            begin
                tcps_kanban:reset_state(),
                tcps_kanban:set_wip_limit(security, Limit),

                Results = [tcps_kanban:process_pull_signal(Order) || Order <- Orders],
                Accepted = length([ok || {ok, _} <- Results]),

                Accepted =< Limit
            end).

%% EUnit wrapper
wip_limit_test_() ->
    {timeout, 60, fun() ->
        ?assert(proper:quickcheck(prop_wip_limit_respected(), [{numtests, 100}]))
    end}.
```

### Testing Map Types

```erlang
%% Generate valid andon contexts
andon_context() ->
    ?LET({SkuId, Stage},
         {binary(), oneof([compilation, testing, validation, execution])},
         #{sku_id => SkuId,
           stage => Stage,
           details => #{error => <<"test error">>}}).

%% Property: Valid contexts are accepted
prop_trigger_andon_accepts_valid_context() ->
    ?FORALL({FailureType, Context},
            {oneof([test_failure, compilation_failure]), andon_context()},
            begin
                Result = tcps_andon:trigger_andon(FailureType, Context),
                case Result of
                    {ok, AndonId} when is_binary(AndonId) -> true;
                    _ -> false
                end
            end).
```

### Shrinking for Better Debugging

```erlang
%% Custom generator with shrinking
work_order_with_shrinking() ->
    ?SHRINK(work_order(), [
        #{bucket => security, priority => 1, payload => #{}},  % Minimal case
        #{bucket => reliability, priority => 5, payload => #{}}
    ]).
```

---

## Dialyzer Configuration

### Recommended rebar.config

```erlang
{dialyzer, [
    {warnings, [
        %% Enabled (strict type checking)
        unmatched_returns,      % Detect ignored function returns
        error_handling,         % Catch missing error cases
        unknown,                % Unknown functions/types
        no_improper_lists,      % List construction errors
        no_fun_app,             % Invalid function applications
        no_match,               % Pattern matching failures
        no_opaque,              % Opaque type violations
        no_fail_call,           % Calls that always fail
        no_contracts,           % Spec violations
        no_behaviours,          % Behaviour callback issues
        no_undefined_callbacks, % Missing callbacks

        %% Optional (can be noisy)
        % no_unused,            % Unused functions
        % no_return,            % Functions that never return
        % race_conditions       % Potential race conditions
    ]},

    %% PLT Configuration
    {plt_apps, top_level_deps},
    {plt_extra_apps, [kernel, stdlib, ssl, inets, jsx, cowboy, bbmustache]},
    {plt_location, local},
    {base_plt_apps, [kernel, stdlib, erts, ssl, inets]},
    {base_plt_location, global},

    %% Exclude specific modules if needed
    % {exclude_mods, [problematic_module]},

    %% Performance
    {get_warnings, true}
]}.
```

### Running Dialyzer

```bash
# Full analysis (slow first time, fast with PLT cache)
rebar3 dialyzer

# Force PLT rebuild
rm -rf _build/default/*_plt && rebar3 dialyzer

# Check specific app
rebar3 dialyzer --apps erlmcp

# Update PLT only (no analysis)
rebar3 dialyzer --update-plt
```

---

## Common Pitfalls

### 1. Over-Specifying with term()

**Bad**:
```erlang
-spec process(term()) -> term().
```

**Good**:
```erlang
-spec process(work_order() | pull_signal()) ->
    {ok, work_order_id()} | {error, error_reason()}.
```

### 2. Forgetting Optional Map Keys

**Bad** (requires all keys):
```erlang
-type config() :: #{
    timeout := integer(),
    retries := integer()
}.
```

**Good** (some keys optional):
```erlang
-type config() :: #{
    timeout => integer(),    % Optional with default
    retries => integer()     % Optional with default
}.
```

### 3. Ambiguous BIF Calls

**Bad** (causes Dialyzer warnings):
```erlang
binary_to_integer(Bin)  % Which BIF? Module override?
```

**Good**:
```erlang
erlang:binary_to_integer(Bin)
```

Or add to module header:
```erlang
-compile({no_auto_import,[binary_to_integer/1]}).
```

### 4. Float Matching in OTP 27

**Bad** (won't match -0.0 in OTP 27):
```erlang
case Value of
    0.0 -> zero;
    _ -> non_zero
end.
```

**Good**:
```erlang
case Value of
    +0.0 -> positive_zero;   % Explicit positive zero
    -0.0 -> negative_zero;   % Explicit negative zero
    _ -> non_zero
end.
```

### 5. Unused Variables in Specs

**Bad** (typo in spec):
```erlang
-spec process(WorkOrder :: work_order()) -> {ok, OrderId :: work_order_id()}.
process(WorkOrd) ->  % Typo! Spec says WorkOrder
    ...
```

**Good**:
```erlang
-spec process(WorkOrder :: work_order()) -> {ok, OrderId :: work_order_id()}.
process(WorkOrder) ->
    ...
```

### 6. Behaviour Callback Conflicts

**Bad** (two behaviours with same callback):
```erlang
-behaviour(gen_server).
-behaviour(erlmcp_transport).  % Both have init/1!
```

**Good** (use delegation):
```erlang
-behaviour(gen_server).

%% Manually implement erlmcp_transport API
init(Args) ->
    %% gen_server init
    erlmcp_transport:init_impl(Args),
    {ok, #state{}}.
```

---

## Examples from TCPS Modules

### tcps_kanban.erl - Excellent Type Coverage

```erlang
-type bucket() :: reliability | security | cost | compliance.
-type work_order_id() :: binary().

-type work_order() :: #{
    id := work_order_id(),
    bucket := bucket(),
    priority := non_neg_integer(),
    created_at := erlang:timestamp(),
    status := pending | in_progress | completed,
    payload := map()
}.

-type pull_signal() :: #{
    bucket := bucket(),
    priority => non_neg_integer(),
    payload := map()
}.

-type leveled_schedule() :: [{bucket(), [work_order()]}].

-type wip_status() :: #{
    current := non_neg_integer(),
    limit := non_neg_integer() | infinity,
    available := non_neg_integer() | infinity,
    utilization := float()
}.

-export_type([bucket/0, work_order_id/0, work_order/0, pull_signal/0,
              leveled_schedule/0, wip_status/0]).

%% All exported functions have specs
-spec start_link() -> {ok, pid()} | {error, term()}.
-spec check_wip_limit(bucket()) -> {ok, available} | {error, limit_reached}.
-spec process_pull_signal(pull_signal()) ->
    {ok, work_order_id()} | {error, limit_reached} | {error, term()}.
```

### tcps_andon.erl - Production-Grade Specs

```erlang
-type failure_type() :: shacl_violation | test_failure | non_determinism |
                        missing_receipt | compilation_failure.

-type andon_event() :: #{
    event_id := andon_event_id(),
    failure_type := failure_type(),
    sku_id := sku_id(),
    stage := stage(),
    timestamp := integer(),
    details := map(),
    status := open | resolved,
    resolution => resolution(),
    metadata => map()
}.

-spec trigger_andon(failure_type(), andon_context()) ->
    {ok, andon_event_id()} | {error, term()}.

-spec resolve_andon(andon_event_id(), resolution()) -> ok | {error, term()}.

-spec is_blocked(sku_id()) -> boolean().

-spec can_proceed_to_stage(sku_id(), stage()) ->
    {ok, proceed} | {blocked, [andon_event_id()]}.
```

### tcps_work_order.erl - Comprehensive Type Definitions

```erlang
-type work_order_id() :: binary().
-type sku_id() :: binary().
-type bucket() :: reliability | security | cost | compliance | features | technical_debt.
-type priority() :: 1..10.
-type stage() :: requirements | design | implementation | testing |
                 integration | deployment | published.

-type work_order_status() :: pending | queued | in_progress | blocked |
                             completed | cancelled.

-type pull_signal() :: #{
    type := github_issue | github_pr | cve | marketplace_install |
            marketplace_refund | internal_request,
    source := binary(),
    description := binary(),
    labels => [binary()],
    metadata => map()
}.

-type sla_status() :: on_time | warning | breached.

-type sla_check() :: #{
    work_order_id := work_order_id(),
    status := sla_status(),
    deadline := erlang:timestamp(),
    remaining_hours => float(),
    overdue_hours => float()
}.

%% Complex function with detailed spec
-spec create_work_order(PullSignal :: pull_signal()) ->
    {ok, work_order_id()} | {error, term()}.

-spec check_sla(WorkOrderId :: work_order_id()) ->
    {ok, on_time} | {warning, float()} | {breached, float()} | {error, not_found}.
```

---

## Conclusion

Type specifications are **documentation, contracts, and verification** rolled into one. By following these best practices:

1. ✅ **Dialyzer catches bugs** at compile time
2. ✅ **Code is self-documenting** with clear contracts
3. ✅ **Refactoring is safer** with type guarantees
4. ✅ **API changes break early** rather than in production
5. ✅ **Property-based tests** verify specs automatically

**Target**: 100% spec coverage on all exported functions.

**Current TCPS Coverage**:
- ✅ tcps_kanban: 100%
- ✅ tcps_andon: 100%
- ✅ tcps_work_order: 100%
- ⚠️ erlmcp_server: 0%
- ⚠️ erlmcp_client: 0%

**Next Steps**:
1. Add specs to erlmcp core modules
2. Run `rebar3 dialyzer` in CI/CD
3. Fix all high-priority warnings
4. Add PropEr property tests

---

**Maintained By**: Type Safety and Dialyzer Specialist Team
**Last Updated**: 2026-01-26
**Version**: 1.0
