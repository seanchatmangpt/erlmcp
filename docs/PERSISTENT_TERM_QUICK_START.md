# persistent_term Quick Start Guide

## 30-Second Start

```erlang
% 1. Start the servers (done automatically via supervision)
{ok, _} = erlmcp_config:start_link(),
{ok, _} = erlmcp_schema_cache:start_link(),

% 2. Read config (10ns - zero copy)
MaxSize = erlmcp_config:get(max_message_size),
Timeout = erlmcp_config:get(request_timeout, 30000),

% 3. Cache a schema
Schema = #{<<"type">> => <<"object">>},
erlmcp_schema_cache:cache_schema(my_schema, Schema),

% 4. Validate data
ok = erlmcp_schema_cache:validate(my_schema, #{key => value}).
```

## Real-World Example: MCP Message Handler

```erlang
-module(erlmcp_message_handler).
-export([handle_message/1]).

handle_message(Message) ->
    % Fast config reads (no gen_server calls!)
    MaxSize = erlmcp_config:get(max_message_size),
    Timeout = erlmcp_config:get(request_timeout),

    % Size check
    case byte_size(Message) of
        Size when Size > MaxSize ->
            {error, message_too_large};
        _ ->
            % Fast schema validation
            case erlmcp_schema_cache:validate(message_schema, Message) of
                ok -> process_message(Message, Timeout);
                {error, Reason} -> {error, {validation_failed, Reason}}
            end
    end.
```

## Common Patterns

### Pattern 1: Configuration-Driven Behavior

```erlang
% Instead of hardcoded values:
handle_request(Request) ->
    process_with_timeout(Request, 30000).  % Hardcoded!

% Use config:
handle_request(Request) ->
    Timeout = erlmcp_config:get(request_timeout),
    process_with_timeout(Request, Timeout).
```

### Pattern 2: Schema-Based Validation

```erlang
% Cache schemas on startup
init([]) ->
    Schemas = [
        {user_schema, load_user_schema()},
        {tool_schema, load_tool_schema()},
        {resource_schema, load_resource_schema()}
    ],
    lists:foreach(fun({Name, Schema}) ->
        erlmcp_schema_cache:cache_schema(Name, Schema)
    end, Schemas),
    {ok, #state{}}.

% Fast validation in hot path
handle_call({create_user, Data}, _From, State) ->
    case erlmcp_schema_cache:validate(user_schema, Data) of
        ok -> create_user(Data);
        {error, Reason} -> {error, {invalid_user, Reason}}
    end.
```

### Pattern 3: Bulk Config Updates

```erlang
% Bad: Multiple GC events
update_config_bad(Updates) ->
    lists:foreach(fun({Key, Value}) ->
        erlmcp_config:set(Key, Value)  % GC per update!
    end, Updates).

% Good: Single GC event
update_config_good(Updates) ->
    erlmcp_config:update(maps:from_list(Updates)).  % One GC
```

## Performance Tips

### 1. Read Often, Write Rarely ✅

```erlang
% Perfect use case
handle_message(Msg) ->
    % Read config 1M times/sec (fast!)
    MaxSize = erlmcp_config:get(max_message_size),
    validate(Msg, MaxSize).

% Update config once per day (rare)
daily_config_refresh() ->
    NewConfig = load_from_database(),
    erlmcp_config:update(NewConfig).
```

### 2. Cache Compiled Schemas ✅

```erlang
% Cache once at startup
init([]) ->
    CompiledSchema = compile_schema(SchemaJson),
    erlmcp_schema_cache:cache_schema(name, CompiledSchema),
    {ok, #state{}}.

% Use millions of times (10ns access)
handle_data(Data) ->
    erlmcp_schema_cache:validate(name, Data).
```

### 3. Avoid Dynamic Keys ❌

```erlang
% Bad: Don't use for per-request data
handle_request(RequestId, Data) ->
    erlmcp_config:set(RequestId, Data),  % Wrong! Too dynamic
    process(Data).

% Good: Use ETS or process state
handle_request(RequestId, Data) ->
    ets:insert(requests, {RequestId, Data}),  % Right!
    process(Data).
```

## Integration Checklist

- [ ] Add to supervision tree (`erlmcp_core_sup.erl`)
- [ ] Load initial config in `init/1`
- [ ] Replace hardcoded config with `get/1` calls
- [ ] Cache schemas on startup
- [ ] Replace ETS config lookups
- [ ] Run benchmarks
- [ ] Verify GC impact is minimal

## Troubleshooting

### Problem: `badarg` error on get/1

```erlang
% Error: Key not found
erlmcp_config:get(missing_key).  % crashes!

% Solution: Use get/2 with default
erlmcp_config:get(missing_key, default_value).  % safe
```

### Problem: High GC pressure

```erlang
% Bad: Too many updates
lists:foreach(fun(K) -> erlmcp_config:set(K, V) end, Keys).

% Good: Bulk update
erlmcp_config:update(maps:from_list([{K, V} || K <- Keys])).
```

### Problem: Schema not found

```erlang
% Error: Schema not cached
erlmcp_schema_cache:validate(unknown_schema, Data).

% Solution: Cache it first
erlmcp_schema_cache:cache_schema(unknown_schema, Schema),
erlmcp_schema_cache:validate(unknown_schema, Data).
```

## Monitoring

```erlang
% Check schema cache stats
Stats = erlmcp_schema_cache:get_stats(),
#{
    cache_hits => Hits,
    cache_misses => Misses,
    hit_rate => HitRate,
    validations => Count
} = Stats,

% Alert if hit rate < 90%
case HitRate < 0.9 of
    true -> logger:warning("Low schema cache hit rate: ~p", [HitRate]);
    false -> ok
end.
```

## Summary

| Feature | Performance | Use Case |
|---------|-------------|----------|
| `erlmcp_config:get/1` | ~10ns | Read config in hot path |
| `erlmcp_config:update/1` | Bulk write | Batch config updates |
| `erlmcp_schema_cache:get_schema/1` | ~10ns | Get cached schema |
| `erlmcp_schema_cache:validate/2` | ~10ns + validation | Validate data fast |

**Key Takeaway**: 100x faster reads, zero message passing, perfect for hot-path configuration! 🚀
