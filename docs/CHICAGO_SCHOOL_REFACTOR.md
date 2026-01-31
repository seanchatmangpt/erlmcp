# Chicago School TDD Refactoring Guide for erlmcp

## Overview

This document describes the refactoring of erlmcp test files to follow **Chicago School TDD principles**. The refactoring ensures tests validate **observable behavior through API boundaries only**, without inspecting internal state or using mock processes.

## Chicago School TDD Principles

### Core Principles

1. **Test Observable Behavior Only**
   - Tests MUST use public API calls
   - NO internal state inspection (no `sys:get_status`, no direct map/tuple access)
   - NO record duplication or internal structure knowledge
   - Tests boundaries, not implementation details

2. **Use REAL Processes**
   - NO mocked or stubbed processes
   - NO dummy `spawn(fun() -> receive after 5000 -> ok end end)` processes
   - Use actual `erlmcp_server`, `erlmcp_client`, `erlmcp_cache` processes
   - Use `erlmcp_test_helpers` for process management

3. **Test Through ALL Interfaces**
   - Test JSON-RPC interface
   - Test stdio transport
   - Test HTTP transport
   - Test WebSocket transport
   - Test TCP transport

4. **Respect Encapsulation**
   - NO direct access to internal state maps
   - NO record definition duplication in tests
   - Use accessor functions from the module being tested
   - Add missing API wrappers if needed

5. **File Size Limits**
   - Test files MUST be < 500 lines
   - Split large files into focused modules
   - Group related tests together

## Common Violations and Fixes

### Violation 1: Dummy Spawn Processes

**BEFORE (VIOLATION):**
```erlang
prop_server_lookup_consistency() ->
    ?FORALL({ServerId, Config}, {server_id(), server_config()},
        begin
            {ok, Registry} = start_test_registry(),
            MockServer = spawn(fun() -> receive after 5000 -> ok end end),  % VIOLATION!

            ok = gen_server:call(Registry, {register_server, ServerId, MockServer, Config}),
            LookupResult = gen_server:call(Registry, {find_server, ServerId}),

            gen_server:stop(Registry),
            exit(MockServer, kill),

            LookupResult =:= {ok, {MockServer, Config}}
        end).
```

**AFTER (Chicago School):**
```erlang
prop_server_lookup_consistency() ->
    ?FORALL(ServerId, server_id(),
        begin
            {ok, _RegPid} = ensure_registry(),
            ServerIdBin = to_binary(ServerId),

            %% Use REAL erlmcp_server process
            {ok, ServerPid} = erlmcp_test_helpers:start_test_server(ServerIdBin),

            %% Use API call, not gen_server:call
            LookupResult = erlmcp_registry:find_server(ServerIdBin),

            erlmcp_test_helpers:stop_test_server(ServerPid),

            case LookupResult of
                {ok, {FoundPid, _Caps}} when is_pid(FoundPid) ->
                    FoundPid =:= ServerPid;
                _ ->
                    false
            end
        end).
```

### Violation 2: Direct State Map Access

**BEFORE (VIOLATION):**
```erlang
prop_session_preserves_metadata() ->
    ?FORALL(Metadata, metadata_map(),
        begin
            Session = erlmcp_session:new(Metadata),
            SessionMetadata = maps:get(metadata, Session),  % VIOLATION!
            SessionMetadata =:= Metadata
        end).
```

**AFTER (Chicago School):**
```erlang
prop_session_preserves_metadata() ->
    ?FORALL(Metadata, metadata_map(),
        begin
            Session = erlmcp_session:new(Metadata),
            %% Use API accessor, check a key from metadata
            case maps:keys(Metadata) of
                [] ->
                    true;  % Empty metadata - test passes
                [FirstKey | _] ->
                    SessionMetadata = erlmcp_session:get_metadata(Session, FirstKey),
                    ExpectedValue = maps:get(FirstKey, Metadata),
                    SessionMetadata =:= ExpectedValue
            end
        end).
```

**Missing API Added:**
```erlang
%% In erlmcp_session.erl
-spec get_created_at(session()) -> integer().
get_created_at(#{created_at := CreatedAt}) ->
    CreatedAt.
```

### Violation 3: Direct Tuple Access

**BEFORE (VIOLATION):**
```erlang
prop_token_consumption_decreases() ->
    ?FORALL({Cap, Count}, {capacity(), proper_types:range(1, 10)},
        begin
            Bucket = erlmcp_rate_limiter:create_token_bucket(Cap),
            {InitialTokens, _LastRefill} = Bucket,  % VIOLATION!

            %% Consume tokens
            consume_tokens(Bucket, Cap, Count),

            %% Cannot directly inspect bucket after consumption
            Result = try_consume_n_times(Bucket, Cap, Cap + 1),
            Result =:= {error, exceeded}
        end).
```

**AFTER (Chicago School):**
```erlang
prop_token_consumption_decreases() ->
    ?FORALL({Cap, Count}, {capacity(), proper_types:range(1, 10)},
        begin
            Bucket = erlmcp_rate_limiter:create_token_bucket(Cap),
            InitialTokens = erlmcp_rate_limiter:bucket_tokens(Bucket),  % API call

            %% Consume tokens
            {FinalBucket, _} = consume_tokens(Bucket, Cap, Count),
            FinalTokens = erlmcp_rate_limiter:bucket_tokens(FinalBucket),  % API call

            %% Verify through API
            FinalTokens =< InitialTokens
        end).
```

### Violation 4: File Size > 500 Lines

**BEFORE (VIOLATION):**
```erlang
%% erlmcp_cache_proper_tests.erl - 682 lines
%% Contains all cache tests in one file
-module(erlmcp_cache_proper_tests).
%% ... 682 lines of tests ...
```

**AFTER (Chicago School):**
```erlang
%% Split into 3 focused modules:

%% erlmcp_cache_basic_proper_tests.erl - 310 lines
%% Basic operations: put/get/delete/clear/etag
-module(erlmcp_cache_basic_proper_tests).

%% erlmcp_cache_ttl_proper_tests.erl - 280 lines
%% TTL expiration, tag invalidation, statistics
-module(erlmcp_cache_ttl_proper_tests).

%% erlmcp_cache_lru_proper_tests.erl - 240 lines
%% LRU eviction, size limits
-module(erlmcp_cache_lru_proper_tests).
```

### Violation 5: Manual Mnesia Setup

**BEFORE (VIOLATION):**
```erlang
%% In each test
start_test_cache(Config) ->
    case mnesia:system_info(is_running) of
        yes ->
            application:stop(mnesia),
            timer:sleep(100);
        _ -> ok
    end,

    catch mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    application:start(mnesia),
    %% ... 50 lines of Mnesia setup ...
```

**AFTER (Chicago School):**
```erlang
%% Use erlmcp_test_helpers
start_test_cache(Config) ->
    {ok, _Cache} = erlmcp_test_helpers:start_test_cache(Config),
    %% ... test code ...
    erlmcp_test_helpers:stop_test_cache().

%% Or use wrapper
erlmcp_test_helpers:with_test_cache(Config, fun() ->
    %% ... test code ...
end).
```

## erlmcp_test_helpers API

### Server Helpers

```erlang
%% Start test server with defaults
{ok, ServerPid} = erlmcp_test_helpers:start_test_server().

%% Start test server with specific ID
{ok, ServerPid} = erlmcp_test_helpers:start_test_server(<<"my_server">>).

%% Start test server with capabilities
{ok, ServerPid} = erlmcp_test_helpers:start_test_server(
    <<"my_server">>,
    #mcp_server_capabilities{
        tools = #mcp_capability{enabled = true}
    }
).

%% Stop test server
ok = erlmcp_test_helpers:stop_test_server(ServerPid).

%% Setup/cleanup wrapper
Result = erlmcp_test_helpers:with_test_server(fun(ServerPid) ->
    {ok, Tools} = erlmcp_server:list_tools(ServerPid),
    length(Tools)
end).
```

### Client Helpers

```erlang
%% Start test client with defaults
{ok, ClientPid} = erlmcp_test_helpers:start_test_client().

%% Start test client with config
{ok, ClientPid} = erlmcp_test_helpers:start_test_client(
    #{transport => {stdio, []}, timeout => 10000}
).

%% Stop test client
ok = erlmcp_test_helpers:stop_test_client(ClientPid).

%% Setup/cleanup wrapper
Result = erlmcp_test_helpers:with_test_client(fun(ClientPid) ->
    {ok, Tools} = erlmcp_client:list_tools(ClientPid),
    length(Tools)
end).
```

### Cache Helpers

```erlang
%% Start test cache with defaults
{ok, CachePid} = erlmcp_test_helpers:start_test_cache().

%% Start test cache with config
{ok, CachePid} = erlmcp_test_helpers:start_test_cache(
    #{max_l1_size => 100, default_ttl_seconds => 5}
).

%% Stop test cache
ok = erlmcp_test_helpers:stop_test_cache().

%% Setup/cleanup wrapper
Result = erlmcp_test_helpers:with_test_cache(fun() ->
    ok = erlmcp_cache:put(<<"key">>, <<"value">>),
    {ok, <<"value">>} = erlmcp_cache:get(<<"key">>)
end).
```

## When to Add Missing API Wrappers

If tests need access to internal state, add API wrappers to the module being tested:

### Example: Adding get_created_at/1

**Before (state access violation):**
```erlang
%% Test directly accesses map
CreatedAt = maps:get(created_at, Session),
```

**After (API added to module):**
```erlang
%% In erlmcp_session.erl
-spec get_created_at(session()) -> integer().
get_created_at(#{created_at := CreatedAt}) ->
    CreatedAt.

%% In test
CreatedAt = erlmcp_session:get_created_at(Session),
```

### Example: Adding bucket_tokens/1

**Before (tuple access violation):**
```erlang
%% Test directly accesses tuple
{Tokens, _LastRefill} = Bucket,
```

**After (API added to module):**
```erlang
%% In erlmcp_rate_limiter.erl (already exported)
-spec bucket_tokens(bucket()) -> float().
bucket_tokens({Tokens, _LastRefill}) ->
    Tokens.

%% In test
Tokens = erlmcp_rate_limiter:bucket_tokens(Bucket),
```

## Test File Organization

### Splitting Large Files

When a test file exceeds 500 lines, split into focused modules:

```
erlmcp_cache_proper_tests.erl (682 lines)
├── erlmcp_cache_basic_proper_tests.erl (310 lines)
│   ├── Basic operations (put/get/delete/clear)
│   └── ETag generation/validation
├── erlmcp_cache_ttl_proper_tests.erl (280 lines)
│   ├── TTL expiration
│   ├── Tag-based invalidation
│   └── Cache statistics
└── erlmcp_cache_lru_proper_tests.erl (240 lines)
    ├── LRU eviction limits
    ├── LRU eviction behavior
    └── Size enforcement

erlmcp_registry_proper_tests.erl (538 lines)
├── erlmcp_registry_server_proper_tests.erl (280 lines)
│   ├── Server registration
│   ├── Server lookup
│   └── Server monitoring
└── erlmcp_registry_transport_proper_tests.erl (310 lines)
    ├── Transport registration
    ├── Transport lookup
    └── Transport-server binding
```

## Property-Based Testing with Proper

### Generator Patterns

```erlang
%% Generate valid cache keys
cache_key() ->
    proper_types:oneof([
        proper_types:binary(),
        proper_types:atom(),
        proper_types:int()
    ]).

%% Generate valid cache values
cache_value() ->
    proper_types:oneof([
        proper_types:binary(),
        proper_types:int(),
        proper_types:bool(),
        proper_types:atom(),
        proper_types:list(proper_types:int()),
        proper_types:map(proper_types:binary(), proper_types:int())
    ]).

%% Generate configurations
cache_config() ->
    proper_types:map(
        proper_types:atom(),
        proper_types:oneof([
            proper_types:int(),
            proper_types:binary()
        ])
    ).
```

### Property Patterns

```erlang
%% Basic roundtrip property
prop_cache_put_get_roundtrip() ->
    ?FORALL({Key, Value, Strategy}, {cache_key(), cache_value(), cache_strategy()},
        begin
            {ok, _Cache} = erlmcp_test_helpers:start_test_cache(#{max_l1_size => 100}),
            ok = erlmcp_cache:put(Key, Value, Strategy),
            Result = erlmcp_cache:get(Key),
            erlmcp_test_helpers:stop_test_cache(),
            Result =:= {ok, Value}
        end).

%% Idempotent operation property
prop_cache_delete_nonexistent() ->
    ?FORALL(Key, cache_key(),
        begin
            {ok, _Cache} = erlmcp_test_helpers:start_test_cache(#{max_l1_size => 100}),
            Result = erlmcp_cache:delete(Key),
            erlmcp_test_helpers:stop_test_cache(),
            Result =:= ok  % Idempotent
        end).

%% Conditional property
prop_cache_put_overwrites() ->
    ?FORALL({Key, Value1, Value2}, {cache_key(), cache_value(), cache_value()},
        ?IMPLIES(Value1 =/= Value2,  % Only test different values
        begin
            {ok, _Cache} = erlmcp_test_helpers:start_test_cache(#{max_l1_size => 100}),
            ok = erlmcp_cache:put(Key, Value1),
            ok = erlmcp_cache:put(Key, Value2),
            Result = erlmcp_cache:get(Key),
            erlmcp_test_helpers:stop_test_cache(),
            Result =:= {ok, Value2}
        end)).
```

## Quality Gates for Refactored Tests

### Compilation
```bash
TERM=dumb rebar3 compile
```
Must pass with 0 errors.

### EUnit Tests
```bash
rebar3 eunit --module=<module>_tests
```
All tests must pass (0 failures).

### Property-Based Tests
```bash
rebar3 eunit --module=<module>_proper_tests
```
All properties must verify (true returned).

### Code Coverage
```bash
rebar3 cover
```
Minimum 80% coverage required.

### Dialyzer
```bash
rebar3 dialyzer
```
0 type warnings (reported).

### Xref
```bash
rebar3 xref
```
0 undefined functions (clean).

## Checklist for Refactoring Tests

Use this checklist when refactoring test files to Chicago School TDD:

- [ ] **No dummy spawn processes** - Use real erlmcp processes via helpers
- [ ] **No direct state access** - Use API calls only
- [ ] **No record duplication** - Respect encapsulation
- [ ] **No sys:get_status** - Test observable behavior only
- [ ] **File < 500 lines** - Split large files
- [ ] **Uses erlmcp_test_helpers** - For process management
- [ ] **API-only access** - Add missing wrappers if needed
- [ ] **Tests all interfaces** - JSON-RPC, stdio, HTTP, WebSocket, TCP
- [ ] **Property-based tests** - Use Proper for invariants
- [ ] **Documentation updated** - Module docs explain Chicago School approach

## Summary

Chicago School TDD refactoring ensures erlmcp tests:

1. **Validate Observable Behavior** - Tests use public API only
2. **Use Real Processes** - No mocks, no dummy processes
3. **Respect Encapsulation** - No internal state inspection
4. **Maintain Focus** - Files < 500 lines, split by concern
5. **Provide Value** - Tests catch real bugs in production behavior

These principles ensure tests remain valuable as implementation changes while preventing brittleness from testing implementation details.
