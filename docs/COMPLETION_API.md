# Completion API - erlmcp

## Overview

The Completion API provides intelligent argument completion for MCP tools, resources, and prompts. It uses fuzzy matching (Jaro-Winkler similarity) to rank suggestions and supports caching, rate limiting, and streaming for large result sets.

**Module**: `erlmcp_completion` (gen_server)
**Spec**: MCP 2025-11-25 experimental feature
**Status**: Production-ready with performance optimizations

## Architecture

### Design Principles

1. **Pluggable Handlers** - Custom completion logic per reference
2. **Intelligent Ranking** - Jaro-Winkler similarity scoring (threshold: 0.7)
3. **Performance First** - ETS cache with TTL (default: 1 hour)
4. **Rate Limited** - Token bucket per (client, ref) pair
5. **Streaming Support** - Large result sets delivered in chunks

### Supervision Tree

```
erlmcp_server (gen_server)
└── erlmcp_completion (gen_server, transient)
    ├── ETS cache (completion_cache)
    ├── Rate limit tracker (map)
    └── Completion handlers (map)
```

**Lifecycle**: Started by `erlmcp_server` on `init/1`, stopped on `terminate/2`.

### State Record

```erlang
-record(state, {
    handlers :: #{completion_ref() => {handler_fun(), type()}},
    rate_limits :: #{rate_limit_key() => {count(), window_start()}},
    cache :: ets:tid(),              % ETS table for cached results
    cache_ttl :: pos_integer(),      % Default: 3600s (1 hour)
    cache_max_size :: pos_integer(), % Default: 1000 entries
    max_results :: pos_integer(),    % Default: 10
    rate_limit :: pos_integer(),     % Default: 10 req/sec
    ranking_threshold :: float(),    % Default: 0.7
    streams :: #{completion_id() => stream_state()}
}).
```

## API Reference

### Starting Completion Server

```erlang
%% Default configuration
{ok, Pid} = erlmcp_completion:start_link().

%% Custom configuration
{ok, Pid} = erlmcp_completion:start_link(#{
    cache_ttl => 7200,         % 2 hours
    cache_max_size => 5000,    % 5000 cached results
    max_results => 20,         % Top 20 suggestions
    rate_limit => 100,         % 100 req/sec
    ranking_threshold => 0.6   % Lower threshold (more results)
}).
```

### Registering Completion Handlers

```erlang
-spec add_completion_handler(pid(), completion_ref(), handler_fun(), type()) ->
    ok | {error, term()}.

%% Example: Tool argument completion
ToolHandler = fun(Ref, Arg, Context) ->
    case {Ref, maps:get(name, Arg)} of
        {<<"deploy">>, <<"environment">>} ->
            %% Complete environment names
            {ok, [
                #{value => <<"production">>, label => <<"Production Environment">>},
                #{value => <<"staging">>, label => <<"Staging Environment">>},
                #{value => <<"development">>, label => <<"Development Environment">>}
            ]};
        {<<"deploy">>, <<"region">>} ->
            %% Complete AWS regions
            {ok, [
                #{value => <<"us-east-1">>, label => <<"US East (N. Virginia)">>},
                #{value => <<"us-west-2">>, label => <<"US West (Oregon)">>},
                #{value => <<"eu-west-1">>, label => <<"Europe (Ireland)">>}
            ]};
        _ ->
            {ok, []}
    end
end,

ok = erlmcp_completion:add_completion_handler(
    CompletionPid,
    <<"deploy">>,
    ToolHandler,
    <<"tool">>
).
```

### Requesting Completions

```erlang
-spec complete(pid(), completion_ref(), argument(), context()) ->
    {ok, completion_result()} | {error, term()}.

%% Basic completion request
{ok, Result} = erlmcp_completion:complete(
    CompletionPid,
    <<"deploy">>,
    #{name => <<"environment">>, value => <<"prod">>},  % Partial: "prod"
    #{type => <<"tool">>}
).

%% Result structure
Result = #{
    completions => [
        #{value => <<"production">>, label => <<"Production Environment">>, score => 0.95}
    ],
    hasMore => false,
    total => 1
}.
```

### Streaming Completions

For large result sets (e.g., file paths, database records):

```erlang
-spec stream_completion(pid(), completion_ref(), argument(), context()) ->
    {ok, completion_id(), stream_pid()} | {error, term()}.

%% Start streaming
{ok, StreamId, StreamPid} = erlmcp_completion:stream_completion(
    CompletionPid,
    <<"file_picker">>,
    #{name => <<"path">>, value => <<"/home/user/doc">>},
    #{type => <<"resource">>}
).

%% Receive chunks
receive
    {chunk, Chunk} ->
        io:format("Received ~p completions~n", [length(Chunk)]);
    {done, StreamId, FinalResult} ->
        io:format("Stream complete: ~p~n", [FinalResult])
after 30000 ->
    erlmcp_completion:cancel_completion(CompletionPid, StreamId),
    error(timeout)
end.
```

### Caching Behavior

```erlang
%% Cache key: {Ref, Argument, Context}
%% Cache entry: {Key, Result, Expiry}

%% First request - cache miss
{ok, Result1} = erlmcp_completion:complete(Pid, Ref, Arg, Ctx).
%% Handler invoked, result cached

%% Second request (within TTL) - cache hit
{ok, Result2} = erlmcp_completion:complete(Pid, Ref, Arg, Ctx).
%% Result served from cache, handler not invoked

%% After TTL expiry - cache miss again
timer:sleep(3601000),  % Wait >1 hour
{ok, Result3} = erlmcp_completion:complete(Pid, Ref, Arg, Ctx).
%% Handler invoked again
```

### Manual Cache Management

```erlang
%% Get cached result
{ok, CachedResult} = erlmcp_completion:get_cached_completion(Pid, Ref, Arg).
%% => Returns cached value or `not_found`

%% Invalidate cache (e.g., after handler update)
ok = erlmcp_completion:remove_completion_handler(Pid, Ref).
%% Cache entries for this ref are invalidated
```

## Custom Completers

### File Path Completer

```erlang
file_path_handler(Ref, Arg, _Context) ->
    PartialPath = maps:get(value, Arg, <<>>),
    case file:list_dir(filename:dirname(PartialPath)) of
        {ok, Files} ->
            Completions = [#{
                value => list_to_binary(filename:join(filename:dirname(PartialPath), F)),
                label => list_to_binary(F)
            } || F <- Files],
            {ok, Completions};
        {error, _} ->
            {ok, []}
    end.
```

### Database Record Completer

```erlang
db_record_handler(<<"users">>, Arg, _Context) ->
    Partial = maps:get(value, Arg, <<>>),
    %% Query database for matching users
    Query = "SELECT id, name, email FROM users WHERE name LIKE $1 LIMIT 20",
    {ok, _, Rows} = epgsql:equery(DbConn, Query, [<<Partial/binary, "%">>]),
    Completions = [#{
        value => Id,
        label => <<Name/binary, " (", Email/binary, ")">>
    } || {Id, Name, Email} <- Rows],
    {ok, Completions}.
```

### API Endpoint Completer

```erlang
api_endpoint_handler(<<"api_call">>, Arg, _Context) ->
    Partial = maps:get(value, Arg, <<>>),
    %% Static list of API endpoints
    Endpoints = [
        #{value => <<"/api/v1/users">>, label => <<"User Management API">>},
        #{value => <<"/api/v1/orders">>, label => <<"Order Management API">>},
        #{value => <<"/api/v1/products">>, label => <<"Product Catalog API">>}
    ],
    {ok, Endpoints}.
```

### Context-Aware Completer

```erlang
context_aware_handler(<<"deploy">>, Arg, Context) ->
    ArgName = maps:get(name, Arg),
    %% Use context from other arguments
    OtherArgs = maps:get(arguments, Context, #{}),

    case {ArgName, OtherArgs} of
        {<<"instance_type">>, #{<<"cloud_provider">> := <<"aws">>}} ->
            %% AWS instance types
            {ok, [
                #{value => <<"t3.micro">>, label => <<"T3 Micro (2 vCPU, 1GB RAM)">>},
                #{value => <<"t3.small">>, label => <<"T3 Small (2 vCPU, 2GB RAM)">>}
            ]};
        {<<"instance_type">>, #{<<"cloud_provider">> := <<"gcp">>}} ->
            %% GCP machine types
            {ok, [
                #{value => <<"e2-micro">>, label => <<"E2 Micro (2 vCPU, 1GB RAM)">>},
                #{value => <<"e2-small">>, label => <<"E2 Small (2 vCPU, 2GB RAM)">>}
            ]};
        _ ->
            {ok, []}
    end.
```

## Jaro-Winkler Similarity Ranking

### Algorithm Overview

The Jaro-Winkler algorithm measures similarity between two strings:
- **Jaro Similarity**: Considers matching characters and transpositions
- **Winkler Modification**: Boosts score for common prefixes (up to 4 chars)

**Range**: 0.0 (no similarity) to 1.0 (exact match)

### Example Scoring

```erlang
%% Input: value = <<"prod">>
%% Candidates:
erlmcp_completion:jaro_winkler_similarity(<<"prod">>, <<"production">>).
%% => 0.87 (common prefix "prod")

erlmcp_completion:jaro_winkler_similarity(<<"prod">>, <<"staging">>).
%% => 0.0 (no match)

erlmcp_completion:jaro_winkler_similarity(<<"prod">>, <<"product">>).
%% => 0.85 (similar prefix)

%% Results filtered by threshold (0.7):
%% - "production" (0.87) ✓ Included
%% - "product" (0.85) ✓ Included
%% - "staging" (0.0) ✗ Excluded
```

### Tuning Threshold

```erlang
%% Strict matching (fewer results, higher confidence)
{ok, Pid} = erlmcp_completion:start_link(#{ranking_threshold => 0.9}).

%% Relaxed matching (more results, lower confidence)
{ok, Pid} = erlmcp_completion:start_link(#{ranking_threshold => 0.5}).

%% Balanced (default)
{ok, Pid} = erlmcp_completion:start_link(#{ranking_threshold => 0.7}).
```

## Rate Limiting

### Token Bucket Algorithm

**Key**: `{ClientPid, CompletionRef}`
**Window**: 1 second
**Limit**: 10 requests/sec (configurable)

**Behavior**:
```erlang
%% First request in window
erlmcp_completion:complete(Pid, Ref, Arg, Ctx).  % Count: 1/10

%% Subsequent requests
erlmcp_completion:complete(Pid, Ref, Arg, Ctx).  % Count: 2/10
...
erlmcp_completion:complete(Pid, Ref, Arg, Ctx).  % Count: 10/10

%% Exceeds limit
erlmcp_completion:complete(Pid, Ref, Arg, Ctx).
%% => {error, {completion_rate_limited, -32101, <<"Rate limit exceeded">>}}

%% After 1 second, window resets
timer:sleep(1100),
erlmcp_completion:complete(Pid, Ref, Arg, Ctx).  % Count: 1/10
```

### Custom Rate Limits

```erlang
%% High-throughput scenarios
{ok, Pid} = erlmcp_completion:start_link(#{rate_limit => 1000}).  % 1000 req/sec

%% Conservative (public APIs)
{ok, Pid} = erlmcp_completion:start_link(#{rate_limit => 1}).  % 1 req/sec
```

## Error Codes (MCP 2025-11-25)

| Code | Name | Description |
|------|------|-------------|
| -32101 | COMPLETION_RATE_LIMITED | Too many requests |
| -32102 | COMPLETION_REF_NOT_FOUND | No handler registered |
| -32103 | COMPLETION_INVALID_ARGUMENT | Invalid argument format |
| -32104 | COMPLETION_HANDLER_FAILED | Handler crashed or returned error |

**Example Error Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32102,
    "message": "Completion reference not found",
    "data": {
      "ref": "unknown_tool"
    }
  }
}
```

## Performance Optimization

### Cache Eviction Strategy

**LRU Approximation**:
- When cache reaches `max_size`, evict 10% oldest entries
- Oldest = first entries in ETS fold (insertion order approximation)
- Avoids full LRU overhead while maintaining reasonable eviction

**Example**:
```erlang
%% Cache size: 1000 entries (max)
%% New entry arrives → triggers eviction
%% Evicts: 100 oldest entries (10%)
%% New size: 901 entries
```

### Concurrency

**ETS Concurrency Flags**:
```erlang
ets:new(completion_cache, [
    set,
    public,                  % Accessible by all processes
    {read_concurrency, true},   % Optimizes concurrent reads
    {write_concurrency, true}   % Optimizes concurrent writes
]).
```

**Expected Performance**:
- **Reads**: ~5M ops/sec (cache hits)
- **Writes**: ~100K ops/sec (cache misses)
- **Handler invocations**: Depends on handler complexity

## Integration Examples

### MCP Server Integration

```erlang
%% In erlmcp_server:init/1
init(Options) ->
    %% Start completion server
    {ok, CompletionPid} = erlmcp_completion:start_link(#{
        cache_ttl => 3600,
        max_results => 10,
        rate_limit => 10
    }),

    %% Register default handlers
    register_default_completers(CompletionPid),

    State = #state{
        completion_handler = CompletionPid,
        ...
    },
    {ok, State}.

%% In erlmcp_server:handle_request/2
handle_request(#{
    <<"method">> := <<"completion/complete">>,
    <<"params">> := Params
}, State) ->
    Ref = maps:get(<<"ref">>, Params),
    Arg = maps:get(<<"argument">>, Params),
    Context = maps:get(<<"arguments">>, Params, #{}),

    case erlmcp_completion:complete(State#state.completion_handler, Ref, Arg, Context) of
        {ok, Result} ->
            {ok, Result, State};
        {error, {completion_rate_limited, Code, Msg}} ->
            {error, #{code => Code, message => Msg}, State};
        {error, {completion_ref_not_found, Code, Msg}} ->
            {error, #{code => Code, message => Msg}, State}
    end.
```

### Dynamic Handler Registration

```erlang
%% Tool defines its own completer when registered
-module(my_deployment_tool).

-export([register/1]).

register(ServerPid) ->
    %% Get completion handler from server state
    CompletionPid = erlmcp_server:get_completion_handler(ServerPid),

    %% Register custom completer
    Handler = fun(<<"deploy">>, Arg, Ctx) ->
        complete_deployment_args(Arg, Ctx)
    end,

    erlmcp_completion:add_completion_handler(
        CompletionPid,
        <<"deploy">>,
        Handler,
        <<"tool">>
    ).

complete_deployment_args(#{name := <<"environment">>}, _Ctx) ->
    Envs = get_available_environments(),
    {ok, [#{value => E, label => E} || E <- Envs]};
complete_deployment_args(#{name := <<"version">>}, _Ctx) ->
    Versions = get_deployment_versions(),
    {ok, [#{value => V, label => V} || V <- Versions]};
complete_deployment_args(_, _) ->
    {ok, []}.
```

## Testing

### Unit Test Example

```erlang
completion_ranking_test() ->
    {ok, Pid} = erlmcp_completion:start_link(#{ranking_threshold => 0.7}),

    Handler = fun(_, _, _) ->
        {ok, [
            #{value => <<"production">>, label => <<"Production">>},
            #{value => <<"staging">>, label => <<"Staging">>},
            #{value => <<"development">>, label => <<"Development">>}
        ]}
    end,

    erlmcp_completion:add_completion_handler(Pid, <<"test">>, Handler, <<"tool">>),

    {ok, Result} = erlmcp_completion:complete(
        Pid,
        <<"test">>,
        #{name => <<"env">>, value => <<"prod">>},
        #{}
    ),

    Completions = maps:get(completions, Result),
    ?assertEqual(1, length(Completions)),  % Only "production" matches
    [First | _] = Completions,
    ?assertEqual(<<"production">>, maps:get(value, First)),
    ?assert(maps:get(score, First) > 0.7).
```

## Monitoring

### Metrics

```erlang
%% Collect completion metrics
erlmcp_metrics:increment(completion_requests, #{ref => Ref}),
erlmcp_metrics:increment(completion_cache_hits, #{ref => Ref}),
erlmcp_metrics:increment(completion_cache_misses, #{ref => Ref}),
erlmcp_metrics:observe(completion_latency_ms, Latency, #{ref => Ref}).
```

### Health Checks

```erlang
completion_health_check() ->
    Stats = #{
        cache_size => ets:info(CompletionCache, size),
        cache_memory => ets:info(CompletionCache, memory),
        active_handlers => maps:size(Handlers),
        rate_limit_entries => maps:size(RateLimits)
    },
    {ok, Stats}.
```

## Future Enhancements

1. **Machine Learning** - Train models on completion usage patterns
2. **Multi-language** - Support for non-English completions
3. **Fuzzy Typo Correction** - "porduction" → "production"
4. **Semantic Ranking** - Beyond string similarity
5. **Collaborative Filtering** - Learn from user selections
