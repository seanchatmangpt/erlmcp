# Dialyzer Warning Fixes

This directory contains example fixes for Dialyzer warnings.

## Contents

### `erlmcp_pricing_state.erl.fixed`
**Example fix demonstrating**: Unmatched expression pattern

**Warnings eliminated**: 15 in this file
- All related to `ets:insert/2` return values

**Pattern used**:
```erlang
% BEFORE:
some_function(Args) ->
    ets:insert(Table, Data).
    % Dialyzer: Expression produces value but is unmatched
    % Also: Success typing returns 'true' but spec says 'ok'

% FIX OPTION 1: Explicit match and convert
some_function(Args) ->
    true = ets:insert(Table, Data),
    ok.

% FIX OPTION 2: Suppress with underscore (if ok not needed)
some_function(Args) ->
    _ = ets:insert(Table, Data),
    ok.

% FIX OPTION 3: Update spec to match actual return
-spec some_function(Args) -> true.  % Instead of 'ok'
some_function(Args) ->
    ets:insert(Table, Data).
```

**Chosen approach**: Option 1 (explicit match and convert)
- Reason: Maintains API consistency (functions returning 'ok')
- Benefit: Clear intent and type safety

**Applicable to**:
- All 42 unmatched expression warnings
- Files: `erlmcp_batch.erl`, `erlmcp_cache.erl`, `erlmcp_circuit_breaker.erl`, etc.
- See `../DIALYZER_FIX_ANALYSIS.md` for complete list

## How to Apply

### For erlmcp_pricing_state.erl

```bash
# Backup original
cp apps/erlmcp_core/src/pricing/erlmcp_pricing_state.erl \
   apps/erlmcp_core/src/pricing/erlmcp_pricing_state.erl.bak

# Apply fix
cp FIXES/erlmcp_pricing_state.erl.fixed \
   apps/erlmcp_core/src/pricing/erlmcp_pricing_state.erl

# Verify
rebar3 compile
rebar3 dialyzer --module=erlmcp_pricing_state
rebar3 eunit --module=erlmcp_pricing_state_tests  # if exists

# If clean, commit
git add apps/erlmcp_core/src/pricing/erlmcp_pricing_state.erl
git commit -m "fix(dialyzer): eliminate 15 warnings in erlmcp_pricing_state

- Fix unmatched ets:insert/2 return values
- Convert true returns to ok to match type specs
- Warnings: 583 → 568 (-15)"
```

### For Other Files

Use the pattern from `erlmcp_pricing_state.erl.fixed` as a template:

1. Identify unmatched expressions in target file
2. For each `ets:insert/update_counter/delete` call:
   - If function spec says returns `ok`: use `true = ets:op(...), ok`
   - If function doesn't return value: use `_ = ets:op(...)`
3. For `gen_server:call/cast` or other operations:
   - If error handling needed: use proper pattern matching
   - If fire-and-forget: use `_ = operation(...)`

## Testing

After applying fixes:

```bash
# Compile
rebar3 compile

# Check dialyzer for this module only
rebar3 dialyzer --module=<module_name>

# Run unit tests
rebar3 eunit --module=<module_name>_tests

# Run full test suite
rebar3 eunit
rebar3 ct

# Check coverage
rebar3 cover
```

## More Fixes Needed

This directory contains only example fixes. For complete fix coverage:

1. See `../DIALYZER_FIX_ANALYSIS.md` for:
   - All 583 warnings cataloged
   - Line-by-line fix recommendations
   - Detailed patterns for each warning type

2. See `../DIALYZER_FIXES_DELIVERABLES.md` for:
   - Overall strategy
   - Phased approach
   - Time estimates

3. See `../DIALYZER_FIX_STATUS.md` for:
   - Current status
   - Blockers
   - Next steps

## Contributing More Fixes

To add more example fixes to this directory:

1. Create a `.fixed` file with fixes applied
2. Add header comments explaining:
   - What warnings were fixed
   - What pattern was used
   - Why this approach was chosen
3. Update this README with the new example
4. Test the fixed version compiles and passes tests

## Fix Pattern Library

### Unmatched ETS Operations

```erlang
% Pattern: ets:insert/2 unmatched
_ = ets:insert(Table, Entry)

% OR if function returns ok
true = ets:insert(Table, Entry), ok
```

### Unmatched gen_server Calls

```erlang
% Pattern: gen_server:call unmatched
_ = gen_server:call(Server, Message)

% OR if you need error handling
case gen_server:call(Server, Message) of
    ok -> ok;
    {error, Reason} -> handle_error(Reason)
end
```

### Unmatched Timer Operations

```erlang
% Pattern: timer:send_after unmatched
_ = timer:send_after(Delay, self(), Message)

% OR if you want to store the timer ref
{ok, TimerRef} = timer:send_after(Delay, self(), Message),
State#state{timer_ref = TimerRef}
```

### Unknown Function Warnings (Profiling)

```erlang
% Pattern: eprof, fprof, cprof functions not found
-ifdef(ENABLE_PROFILING).

profile(Fun) ->
    eprof:start(),
    Result = Fun(),
    eprof:stop(),
    Result.

-else.

profile(Fun) ->
    Fun().

-endif.
```

### Dead Function Warnings

```erlang
% Option 1: Export if part of API
-export([some_function/1]).

% Option 2: Suppress warning
-dialyzer({nowarn_function, [some_function/1]}).

% Option 3: Remove the function if truly unused
```

### Pattern Match Errors

```erlang
% Pattern: Pattern can never match
% BEFORE:
case some_operation() of
    {ok, Result} -> Result;
    {error, Reason} -> error  % But dialyzer knows this is impossible
end.

% AFTER: Remove impossible clause
some_operation()  % Just return the result directly
```

### Type Spec Mismatches

```erlang
% Pattern: Success typing vs spec mismatch
% BEFORE:
-spec update_state(State) -> ok.
update_state(State) ->
    ets:insert(table, State).  % Returns true, not ok!

% FIX Option 1: Match spec
-spec update_state(State) -> ok.
update_state(State) ->
    true = ets:insert(table, State),
    ok.

% FIX Option 2: Update spec
-spec update_state(State) -> true.
update_state(State) ->
    ets:insert(table, State).
```

## Results

Applying these patterns across the codebase will:

- **Phase 1** (quick wins): -120 warnings in 2-3 hours
- **Phase 2** (type fixes): -96 warnings in 3-4 hours
- **Phase 3** (complex): -267+ warnings in 4-6 hours
- **Total**: 583 → <100 warnings ✅

## Questions?

See the main analysis documents in the parent directory for complete details.
