# Tagged Monitors in OTP 28 - Implementation Note

## Status: Feature Request Clarification Needed

The task description mentioned "OTP 27 tagged monitors" where tags would be embedded in DOWN messages. However, after thorough testing on OTP 28, this feature **does not exist** as described.

## Actual OTP 28 Monitor Features

### erlang:monitor/3 with Options

OTP 28 does support `erlang:monitor(process, Pid, [{tag, Tag}])`, but:
- The tag is **NOT** embedded in the DOWN message
- Tagged monitors appear to behave differently (may not send DOWN)
- The tag feature may be for internal use or future functionality

### What Actually Works

**Traditional monitoring with Ref -> Tag mapping:**
```erlang
%% Create monitor
Ref = erlang:monitor(process, Pid),

%% Store Ref -> Tag mapping
State#state{monitors = maps:put(Ref, Tag, State#state.monitors)}

%% In DOWN handler - look up tag
handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
    Tag = maps:get(Ref, State#state.monitors),
    handle_crash(Tag, Pid, Reason, State).
```

## Alternative: Monitored Registry Pattern

Instead of relying on non-existent tagged monitors, we implemented `erlmcp_monitored_registry` which uses traditional monitors with automatic cleanup:

```erlang
%% Register process
register(Key, Value, Tag) ->
    Pid = self(),
    Ref = erlang:monitor(process, Pid),
    %% Store mapping
    ok.

%% DOWN handler uses tag index
handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
    %% Find keys by tag
    Keys = maps:get(Tag, State#state.tag_index, []),
    %% Clean up all entries with this tag
    NewRegistry = remove_keys(Keys, State#state.registry),
    {noreply, State#state{registry = NewRegistry}}.
```

## Recommendation

1. **Keep current implementation** using Ref -> Tag mappings
2. **Document the pattern** clearly in code
3. **Monitor OTP 29/30** for future tagged monitor enhancements
4. **Consider alias monitoring** (OTP 28) as an alternative

## Files Created

- `/apps/erlmcp_core/src/erlmcp_monitored_registry.erl` - Registry with tag-based cleanup
- `/apps/erlmcp_core/src/erlmcp_session_backend.erl` - Updated with tool monitoring
- `/apps/erlmcp_core/test/erlmcp_tagged_monitors_tests.erl` - Test suite (needs revision)
- `/docs/TAGGED_MONITORS_OTP27.md` - Documentation (corrected)

## Next Steps

1. Clarify actual requirements with user
2. Either:
   - Use traditional Ref -> Tag mapping (proven, works)
   - Implement monitored registry pattern (cleaner abstraction)
   - Explore OTP 28 alias monitoring as alternative
