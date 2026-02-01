# erlmcp v3: Resource Expansion Design Plan

## Executive Summary

**Status**: Design Specification
**Version**: 3.0.0
**Author**: Erlang OTP Developer Agent
**Date**: 2026-01-31

**Current Limitations**:
- Resources only support `file://` URIs
- No HTTP/S3/Cloud storage integration
- No streaming resources
- Limited content type support
- Tight coupling between resource management and transport

**Expansion Goals**:
1. Extensible resource handler behavior (plugin architecture)
2. HTTP resource fetcher with caching
3. Streaming resource interface (chunked transfer)
4. Content negotiation (accept headers, transcoding)
5. URI scheme extensibility (s3://, gs://, custom://)

**Success Criteria**:
- Resource handlers are pluggable via behavior contract
- HTTP resources cached with proper TTL validation
- Streaming supports backpressure and chunked responses
- Content negotiation follows RFC 7231
- Zero breaking changes to existing `file://` resources

---

## Architecture Overview

### Current State (v2.1.0)

```
┌────────────────────────────────────────────────────┐
│           erlmcp_resources (gen_server)            │
│  ┌──────────────────────────────────────────────┐  │
│  │  do_read_resource/1                          │  │
│  │    ├─ file:// → file:read_file/1             │  │
│  │    └─ unknown_scheme → {error, unknown}      │  │
│  └──────────────────────────────────────────────┘  │
└────────────────────────────────────────────────────┘
```

**Problems**:
- Hardcoded file:// logic
- No extensibility point for new schemes
- Tight coupling to filesystem
- No caching layer
- No streaming support

### Target State (v3.0.0)

```
┌──────────────────────────────────────────────────────────────────────┐
│                       erlmcp_resource_manager                        │
│                         (gen_server coordinator)                      │
└─────────────┬────────────────────────────────────────────────────────┘
              │
              │ delegates to
              ▼
┌──────────────────────────────────────────────────────────────────────┐
│                    erlmcp_resource_handler_registry                  │
│                       (gproc-based routing)                          │
└─────┬────────┬────────┬────────┬────────┬────────────────────────────┘
      │        │        │        │        │
      │        │        │        │        │
      ▼        ▼        ▼        ▼        ▼
   ┌──────┐ ┌──────┐ ┌──────┐ ┌──────┐ ┌──────┐
   │ file │ │ http │ │  s3  │ │  gs  │ │custom│
   │handler│ │handler│ │handler│ │handler│ │handler│
   └──────┘ └──────┘ └──────┘ └──────┘ └──────┘
      │        │        │        │        │
      │        │        │        │        │
      ▼        ▼        ▼        ▼        ▼
   ┌──────────────────────────────────────────────┐
   │    erlmcp_resource_cache (ETS)                │
   │    ┌────────────────────────────────────┐    │
   │    │ TTL-based cache entries             │    │
   │    │ - content (binary)                  │    │
   │    │ - metadata (map)                    │    │
   │    │ - expires_at (timestamp)            │    │
   │    └────────────────────────────────────┘    │
   └──────────────────────────────────────────────┘
```

**Key Components**:
1. **erlmcp_resource_handler** - Behavior interface for resource handlers
2. **erlmcp_resource_handler_registry** - gproc-based handler routing
3. **erlmcp_resource_cache** - ETS cache with TTL support
4. **erlmcp_resource_stream** - Streaming interface for chunked responses
5. **erlmcp_resource_content_negotiator** - RFC 7231 content negotiation

---

## Component Design

### 1. Resource Handler Behavior

**File**: `apps/erlmcp_core/src/erlmcp_resource_handler.erl`

**Purpose**: Define plugin interface for resource handlers (similar to `erlmcp_transport_behavior`)

**Behavior Contract**:

```erlang
%% @doc Initialize handler with configuration
-callback init(Config :: map()) ->
    {ok, State :: term()} |
    {error, Reason :: term()}.

%% @doc Check if handler can process this URI
-callback can_handle(Uri :: binary()) ->
    boolean().

%% @doc Read resource (blocking, full content)
-callback read_resource(Uri :: binary(), State :: term()) ->
    {ok, Content :: binary(), Metadata :: map()} |
    {error, Reason :: term()}.

%% @doc Read resource stream (non-blocking, chunked)
-callback read_resource_stream(Uri :: binary(), State :: term()) ->
    {ok, StreamPid :: pid()} |
    {error, Reason :: term()}.

%% @doc Get URI scheme this handler supports
-callback uri_scheme() ->
    binary().

%% @doc Validate URI format
-callback validate_uri(Uri :: binary()) ->
    ok | {error, Reason :: term()}.

%% @doc Get handler capabilities
-callback capabilities() ->
    #{
        streaming => boolean(),
        caching => boolean(),
        content_negotiation => boolean(),
        max_size => non_neg_integer() | undefined
    }.

%% @doc Cleanup handler resources
-callback close(State :: term()) ->
    ok.
```

**Optional Callbacks**:

```erlang
%% @doc Get resource metadata without reading content
-callback get_metadata(Uri :: binary(), State :: term()) ->
    {ok, Metadata :: map()} |
    {error, Reason :: term()}.

%% @doc Check if resource has changed (ETag/Last-Modified)
-callback resource_changed(Uri :: binary(), State :: term()) ->
    {ok, boolean()} |
    {error, Reason :: term()}.
```

**Example Implementation**:

```erlang
-module(erlmcp_resource_http_handler).
-behaviour(erlmcp_resource_handler).
-behaviour(gen_server).

%% Handler callbacks
-export([init/1, can_handle/1, read_resource/2, read_resource_stream/2,
         uri_scheme/0, validate_uri/1, capabilities/0, close/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    http_client :: pid(),
    cache_ttl :: non_neg_integer(),
    max_redirects :: non_neg_integer()
}).

uri_scheme() -> <<"http">>.

can_handle(Uri) ->
    case uri_string:parse(Uri) of
        #{scheme := <<"http">>} -> true;
        #{scheme := <<"https">>} -> true;
        _ -> false
    end.

validate_uri(Uri) ->
    case uri_string:parse(Uri) of
        #{scheme := Scheme} when Scheme =:= <<"http">>; Scheme =:= <<"https">> ->
            ok;
        _ ->
            {error, invalid_uri_scheme}
    end.

capabilities() ->
    #{
        streaming => true,
        caching => true,
        content_negotiation => true,
        max_size => 10485760  % 10 MB default
    }.

init(Config) ->
    CacheTTL = maps:get(cache_ttl, Config, 300000),  % 5 minutes
    MaxRedirects = maps:get(max_redirects, Config, 5),
    {ok, GunPid} = gun:open(_, _, #{protocols => [http]}),
    {ok, #state{
        http_client = GunPid,
        cache_ttl = CacheTTL,
        max_redirects = MaxRedirects
    }}.

read_resource(Uri, State) ->
    %% Fetch via gun, handle redirects, cache result
    case http_get(Uri, State) of
        {ok, Content, Headers} ->
            Metadata = extract_metadata(Headers),
            {ok, Content, Metadata};
        {error, Reason} ->
            {error, Reason}
    end.

read_resource_stream(Uri, State) ->
    %% Spawn streaming process
    erlmcp_resource_stream:start_link(Uri, State).

close(_Uri, State) ->
    gun:close(State#state.http_client),
    ok.
```

### 2. Resource Handler Registry

**File**: `apps/erlmcp_core/src/erlmcp_resource_handler_registry.erl`

**Purpose**: gproc-based routing for resource handlers (similar to `erlmcp_registry`)

**API**:

```erlang
%% Register a handler for a URI scheme
-spec register_handler(Scheme :: binary(), Module :: module(), Pid :: pid()) ->
    ok | {error, term()}.

%% Unregister handler
-spec unregister_handler(Scheme :: binary()) ->
    ok.

%% Find handler for URI
-spec find_handler(Uri :: binary()) ->
    {ok, Module :: module(), Pid :: pid()} |
    {error, no_handler}.

%% List registered handlers
-spec list_handlers() ->
    [{Scheme :: binary(), Module :: module()}].
```

**Implementation**:

```erlang
register_handler(Scheme, Module, Pid) ->
    %% Register scheme mapping
    gproc:reg({p, l, {resource_handler, Scheme}}, Module),
    %% Register handler process
    gproc:add_local_name({resource_handler, Scheme}),
    %% Monitor handler process
    monitor(process, Pid),
    ok.

find_handler(Uri) ->
    #{scheme := Scheme} = uri_string:parse(Uri),
    case gproc:lookup_value({p, l, {resource_handler, Scheme}}) of
        {Module, Pid} when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true -> {ok, Module, Pid};
                false -> {error, handler_dead}
            end;
        undefined ->
            {error, no_handler}
    end.
```

### 3. Resource Cache

**File**: `apps/erlmcp_core/src/erlmcp_resource_cache.erl`

**Purpose**: ETS-based caching with TTL support (similar to `erlmcp_session_ets`)

**Cache Entry Format**:

```erlang
-record(cache_entry, {
    key :: {Scheme :: binary(), Uri :: binary()},
    content :: binary(),
    metadata :: map(),
    etag :: binary() | undefined,
    last_modified :: binary() | undefined,
    created_at :: integer(),     % milliseconds
    expires_at :: integer(),     % milliseconds
    access_count :: non_neg_integer(),
    size_bytes :: non_neg_integer()
}).
```

**API**:

```erlang
%% Start cache
-spec start_link(map()) -> {ok, pid()} | {error, term()}.

%% Get cached resource
-spec get(binary()) ->
    {ok, binary(), map()} |
    {error, not_found | expired}.

%% Put resource in cache
-spec put(binary(), binary(), map(), non_neg_integer()) ->
    ok.

%% Invalidate cache entry
-spec invalidate(binary()) ->
    ok.

%% Clean expired entries
-spec cleanup_expired() ->
    {ok, Count :: non_neg_integer()}.

%% Get cache statistics
-spec get_stats() ->
    map().
```

**Configuration**:

```erlang
{erlmcp_resource_cache, [
    {max_entries, 10000},          % Maximum cache entries
    {default_ttl, 300000},          % 5 minutes default TTL
    {cleanup_interval, 60000},      % Clean expired every 60s
    {max_size_bytes, 1073741824}    % 1 GB max cache size
]}.
```

### 4. Resource Stream Interface

**File**: `apps/erlmcp_core/src/erlmcp_resource_stream.erl`

**Purpose**: Streaming resource reader with backpressure (chunked transfer encoding)

**Stream Protocol**:

```erlang
%% Client sends
{'$resource_stream_request', From :: pid(), Uri :: binary(), ChunkSize :: pos_integer()}.

%% Stream replies with chunks
{'$resource_stream_chunk', Uri :: binary(), Chunk :: binary(), Offset :: non_neg_integer()}.

%% Stream completion
{'$resource_stream_complete', Uri :: binary(), TotalBytes :: non_neg_integer()}.

%% Stream error
{'$resource_stream_error', Uri :: binary(), Reason :: term()}.
```

**Implementation**:

```erlang
-module(erlmcp_resource_stream).
-behaviour(gen_server).

-record(state, {
    uri :: binary(),
    handler_pid :: pid(),
    buffer :: binary(),
    offset :: non_neg_integer(),
    chunk_size :: pos_integer(),
    client :: pid()
}).

start_link(Uri, HandlerPid) ->
    gen_server:start_link(?MODULE, [Uri, HandlerPid], []).

init([Uri, HandlerPid]) ->
    {ok, #state{
        uri = Uri,
        handler_pid = HandlerPid,
        offset = 0,
        chunk_size = 65536  % 64 KB chunks
    }}.

handle_call({'$resource_stream_request', Client, Uri, ChunkSize}, _From, State) ->
    {reply, ok, State#state{
        client = Client,
        chunk_size = ChunkSize
    }}.

handle_info({gun_data, _GunPid, _StreamRef, IsFin, Data}, State) ->
    %% Send chunk to client with backpressure
    State#state.client ! {'$resource_stream_chunk',
                          State#state.uri,
                          Data,
                          State#state.offset},
    NewOffset = State#state.offset + byte_size(Data),
    case IsFin of
        fin ->
            State#state.client ! {'$resource_stream_complete',
                                  State#state.uri,
                                  NewOffset},
            {stop, normal, State};
        nofin ->
            {noreply, State#state{offset = NewOffset}}
    end.
```

### 5. Content Negotiator

**File**: `apps/erlmcp_core/src/erlmcp_resource_content_negotiator.erl`

**Purpose**: RFC 7231 content negotiation (Accept headers, transcoding)

**API**:

```erlang
%% Negotiate content type based on Accept header
-spec negotiate_content_type(
    AcceptHeader :: binary(),
    AvailableTypes :: [binary()]
) -> {ok, binary()} | {error, no_acceptable_type}.

%% Parse Accept header
-spec parse_accept_header(binary()) ->
    [{Type :: binary(), Q :: float(), Params :: map()}].

%% Check if content type is acceptable
-spec is_acceptable(binary(), binary()) ->
    boolean().

%% Transcode content if needed
-spec transcode(binary(), binary(), binary()) ->
    {ok, binary()} | {error, term()}.
```

**Implementation**:

```erlang
parse_accept_header(AcceptHeader) ->
    %% Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
    Types = binary:split(AcceptHeader, <<",">>, [global]),
    lists:map(fun parse_media_range/1, Types).

parse_media_range(MediaRange) ->
    %% Parse "type/subtype;q=0.8;param=value"
    [TypeWithParams | _] = binary:split(MediaRange, <<";">>),
    [Type, Subtype] = binary:split(TypeWithParams, <<"/">>),
    Q = extract_q_value(MediaRange, 1.0),
    {<<Type/binary, "/", Subtype/binary>>, Q, #{}}.

negotiate_content_type(AcceptHeader, AvailableTypes) ->
    Parsed = parse_accept_header(AcceptHeader),
    %% Sort by q value
    Sorted = lists:sort(fun({_, Q1, _}, {_, Q2, _}) -> Q1 >= Q2 end, Parsed),
    %% Find first matching type
    find_matching_type(Sorted, AvailableTypes).

find_matching_type([], _AvailableTypes) ->
    {error, no_acceptable_type};
find_matching_type([{Type, _Q, _Params} | Rest], AvailableTypes) ->
    case lists:member(Type, AvailableTypes) of
        true -> {ok, Type};
        false -> find_matching_type(Rest, AvailableTypes)
    end.
```

---

## URI Scheme Extensibility

### Built-in Schemes

| Scheme | Handler Module | Capabilities |
|--------|---------------|--------------|
| `file://` | `erlmcp_resource_file_handler` | Local filesystem |
| `http://` | `erlmcp_resource_http_handler` | HTTP/1.1, HTTP/2 |
| `https://` | `erlmcp_resource_http_handler` | HTTP with TLS |
| `s3://` | `erlmcp_resource_s3_handler` | AWS S3 |
| `gs://` | `erlmcp_resource_gs_handler` | GCP Cloud Storage |

### Custom Scheme Registration

**Example: `custom://` scheme**

```erlang
-module(my_custom_handler).
-behaviour(erlmcp_resource_handler).

uri_scheme() -> <<"custom">>.

can_handle(Uri) ->
    case uri_string:parse(Uri) of
        #{scheme := <<"custom">>} -> true;
        _ -> false
    end.

read_resource(<<"custom://my-resource">>, _State) ->
    {ok, <<"Custom content">>, #{}}.

%% Register handler
{ok, HandlerPid} = my_custom_handler:start_link(#{}),
ok = erlmcp_resource_handler_registry:register_handler(
    <<"custom">>,
    my_custom_handler,
    HandlerPid
).

%% Use resource
{ok, Content, Meta} = erlmcp_resource_manager:read_resource(
    <<"custom://my-resource">>
).
```

### URI Validation

```erlang
%% Validate URI format
validate_uri(Uri) ->
    try
        #{scheme := Scheme, path := Path} = uri_string:parse(Uri),
        case byte_size(Scheme) of
            0 -> {error, missing_scheme};
            _ ->
                case byte_size(Path) of
                    0 -> {error, missing_path};
                    _ -> ok
                end
        end
    catch
        error:badarg -> {error, invalid_uri_format}
    end.
```

---

## HTTP Resource Handler Details

### Features

1. **HTTP/1.1 and HTTP/2 support** (via gun)
2. **Automatic redirect following** (configurable max)
3. **Chunked transfer encoding** (streaming)
4. **ETag/Last-Modified caching** (conditional requests)
5. **Content negotiation** (Accept headers)
6. **Authentication** (Basic, Bearer token)
7. **Timeout handling** (connect, request)
8. **Connection pooling** (via poolboy)

### Configuration

```erlang
{erlmcp_resource_http_handler, [
    {connect_timeout, 5000},         % 5 seconds
    {request_timeout, 30000},        % 30 seconds
    {max_redirects, 5},
    {max_body_size, 10485760},       % 10 MB
    {user_agent, <<"erlmcp/3.0">>},
    {pool_size, 10},
    {pool_max_overflow, 5},
    {auth => #{
        type => bearer,
        token => {env_var, "HTTP_AUTH_TOKEN"}
    }},
    {tls_options => [
        {verify, verify_peer},
        {cacertfile, "/path/to/ca.pem"}
    ]}
]}.
```

### Example Usage

```erlang
%% Read HTTP resource
{ok, Content, Metadata} = erlmcp_resource_http_handler:read_resource(
    <<"https://api.example.com/data.json">>,
    State
).

%% Stream HTTP resource
{ok, StreamPid} = erlmcp_resource_http_handler:read_resource_stream(
    <<"https://example.com/large-file.bin">>,
    State
),
receive
    {'$resource_stream_chunk', _, Chunk, _Offset} ->
        %% Process chunk
        process_chunk(Chunk);
    {'$resource_stream_complete', _, TotalBytes} ->
        logger:info("Streamed ~p bytes", [TotalBytes])
end.
```

### Conditional Requests (Cache Validation)

```erlang
%% Send conditional request based on cached ETag
read_resource_with_cache(Uri, CacheEntry, State) ->
    Headers = case CacheEntry#cache_entry.etag of
        undefined -> [];
        ETag -> [{<<"if-none-match">>, ETag}]
    end,
    case http_get(Uri, Headers, State) of
        {ok, 304, _Headers} ->
            %% Not modified, return cached content
            {ok, CacheEntry#cache_entry.content,
             CacheEntry#cache_entry.metadata};
        {ok, 200, Headers, Content} ->
            %% New content, update cache
            NewETag = get_header(<<"etag">>, Headers),
            {ok, Content, #{etag => NewETag}}
    end.
```

---

## Streaming Resources

### Chunked Transfer Protocol

**Client → Stream Process**:

```erlang
%% Request stream with backpressure
StreamPid ! {'$resource_stream_request', self(), Uri, ChunkSize},
%% Receive chunks
receive
    {'$resource_stream_chunk', Uri, Chunk, Offset} ->
        %% Process chunk (implicit backpressure)
        process_chunk(Chunk),
        %% Next chunk sent automatically after processing
    {'$resource_stream_complete', Uri, TotalBytes} ->
        logger:info("Stream complete: ~p bytes", [TotalBytes])
end.
```

**Stream Process → Handler**:

```erlang
%% Stream pulls data from handler in chunks
handle_info('$get_next_chunk', State) ->
    case erlmcp_resource_handler:read_chunk(
        State#state.uri,
        State#state.offset,
        State#state.chunk_size,
        State#state.handler_pid
    ) of
        {ok, Chunk, IsEof} ->
            State#state.client ! {'$resource_stream_chunk',
                                  State#state.uri,
                                  Chunk,
                                  State#state.offset},
            case IsEof of
                true ->
                    State#state.client ! {'$resource_stream_complete',
                                          State#state.uri,
                                          State#state.offset + byte_size(Chunk)},
                    {stop, normal, State};
                false ->
                    %% Request next chunk
                    self() ! '$get_next_chunk',
                    {noreply, State#state{offset = State#state.offset + byte_size(Chunk)}}
            end;
        {error, Reason} ->
            State#state.client ! {'$resource_stream_error',
                                  State#state.uri,
                                  Reason},
            {stop, {error, Reason}, State}
    end.
```

### Backpressure

Backpressure is implicit in the stream protocol:
- Stream process only sends next chunk after receiving message from client
- Client controls chunk size via request parameter
- If client crashes, stream process detects via monitor and terminates

### Example: Streaming Large File

```erlang
%% Start stream
{ok, StreamPid} = erlmcp_resource_stream:start_link(
    <<"https://example.com/large-file.bin">>,
    HandlerPid
),

%% Request stream with 1 MB chunks
StreamPid ! {'$resource_stream_request', self(),
             <<"https://example.com/large-file.bin">>, 1048576},

%% Receive and write chunks to file
{ok, Fd} = file:open("large-file.bin", [write, binary]),
loop_stream(Fd, StreamPid, 0).

loop_stream(Fd, StreamPid, BytesWritten) ->
    receive
        {'$resource_stream_chunk', _, Chunk, _Offset} ->
            ok = file:write(Fd, Chunk),
            loop_stream(Fd, StreamPid, BytesWritten + byte_size(Chunk));
        {'$resource_stream_complete', _, TotalBytes} ->
            ok = file:close(Fd),
            logger:info("Wrote ~p bytes", [TotalBytes]);
        {'$resource_stream_error', _, Reason} ->
            logger:error("Stream error: ~p", [Reason]),
            ok = file:close(Fd)
    after 30000 ->
        logger:error("Stream timeout"),
        ok = file:close(Fd)
    end.
```

---

## Content Negotiation

### Accept Header Parsing

```erlang
%% Parse Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
parse_accept_header(<<"text/html,application/xml;q=0.9,*/*;q=0.8">>) ->
    [
        {<<"text/html">>, 1.0, #{}},
        {<<"application/xml">>, 0.9, #{}},
        {<<"*/*">>, 0.8, #{}}
    ].
```

### Content Type Matching

```erlang
%% Match specific type
match_type(<<"text/html">>, [<<"text/html">>]) -> true;
match_type(<<"text/html">>, [<<"*/*">>]) -> true;
match_type(<<"text/html">>, [<<"text/*">>]) -> true;
match_type(<<"text/html">>, [<<"application/json">>]) -> false.
```

### Transcoding

```erlang
%% Transcode JSON to YAML if needed
transcode(Content, <<"application/json">>, <<"application/x-yaml">>) ->
    {ok, JSON} = jsx:decode(Content),
    {ok, YAML} = yamerl:encode(JSON),
    {ok, iolist_to_binary(YAML)};
transcode(Content, SourceType, TargetType) ->
    {error, {no_transcoder, SourceType, TargetType}}.
```

---

## Migration Path (v2.1.0 → v3.0.0)

### Phase 1: Add Behavior Interface (Non-Breaking)

**Goal**: Introduce `erlmcp_resource_handler` behavior without changing existing code.

**Actions**:
1. Create `erlmcp_resource_handler.erl` behavior
2. Refactor `erlmcp_resources.erl` to implement behavior
3. Add `erlmcp_resource_handler_registry.erl` (new module)
4. Register `file://` handler in application startup

**Compatibility**: 100% backward compatible

### Phase 2: Add HTTP Handler (Additive)

**Goal**: Add HTTP resource support while keeping file:// working.

**Actions**:
1. Create `erlmcp_resource_http_handler.erl`
2. Add gun dependency to `rebar.config`
3. Update application supervisor to include HTTP handler
4. Add configuration for HTTP handler

**Compatibility**: Existing file:// resources unchanged

### Phase 3: Add Cache Layer (Performance)

**Goal**: Add caching for all resource handlers.

**Actions**:
1. Create `erlmcp_resource_cache.erl`
2. Update `erlmcp_resource_manager` to check cache first
3. Add TTL configuration per handler
4. Add cache statistics API

**Compatibility**: Cache is optional (can be disabled)

### Phase 4: Add Streaming (Additive)

**Goal**: Add streaming support for large resources.

**Actions**:
1. Create `erlmcp_resource_stream.erl`
2. Update handlers to implement `read_resource_stream/2`
3. Add stream protocol documentation
4. Add streaming examples

**Compatibility**: Streaming is opt-in (default is blocking read)

### Phase 5: Add Content Negotiation (Additive)

**Goal**: Add RFC 7231 content negotiation.

**Actions**:
1. Create `erlmcp_resource_content_negotiator.erl`
2. Update HTTP handler to send Accept headers
3. Add transcoding support (optional)
4. Update documentation

**Compatibility**: Content negotiation is opt-in

---

## Testing Strategy

### Unit Tests (EUnit)

```erlang
%% Test handler registry
register_handler_test() ->
    {ok, Pid} = mock_handler:start_link(),
    ok = erlmcp_resource_handler_registry:register_handler(
        <<"test">>, mock_handler, Pid
    ),
    ?assertEqual({ok, mock_handler, Pid},
                 erlmcp_resource_handler_registry:find_handler(
                     <<"test://resource">>
                 )).

%% Test cache with TTL
cache_ttl_test() ->
    {ok, CachePid} = erlmcp_resource_cache:start_link(#{}),
    ok = erlmcp_resource_cache:put(
        <<"test://uri">>, <<"content">>, #{}, 100
    ),
    timer:sleep(150),
    ?assertEqual({error, expired},
                 erlmcp_resource_cache:get(<<"test://uri">>)).

%% Test content negotiation
content_negotiation_test() ->
    Accept = <<"text/html,application/xml;q=0.9,*/*;q=0.8">>,
    Available = [<<"application/json">>, <<"*/*">>],
    ?assertEqual({ok, <<"*/*">>},
                 erlmcp_resource_content_negotiator:negotiate_content_type(
                     Accept, Available
                 )).
```

### Integration Tests (Common Test)

```erlang
%% Test HTTP resource fetch
http_resource_fetch_test(_Config) ->
    %% Start mock HTTP server
    {ok, _ServerPid} = start_mock_http_server(8080),
    {ok, HandlerPid} = erlmcp_resource_http_handler:start_link(#{}),

    %% Fetch resource
    {ok, Content, Metadata} = erlmcp_resource_handler:read_resource(
        <<"http://localhost:8080/test.json">>,
        HandlerPid
    ),

    ?assertEqual(<<"{\"status\":\"ok\"}">>, Content),
    ?assertMatch(#{<<"content-type">> := <<"application/json">>}, Metadata).

%% Test streaming
stream_large_resource_test(_Config) ->
    {ok, StreamPid} = erlmcp_resource_stream:start_link(
        <<"http://localhost:8080/large.bin">>,
        HandlerPid
    ),
    StreamPid ! {'$resource_stream_request', self(),
                 <<"http://localhost:8080/large.bin">>, 1024},

    %% Verify chunks received
    Chunks = receive_chunks(5, 1000),
    ?assertEqual(5, length(Chunks)),
    ?assertEqual(5120, lists:sum([byte_size(C) || C <- Chunks])).

receive_chunks(Count, Timeout) ->
    receive_chunks(Count, Timeout, []).

receive_chunks(0, _Timeout, Acc) ->
    lists:reverse(Acc);
receive_chunks(Count, Timeout, Acc) ->
    receive
        {'$resource_stream_chunk', _, Chunk, _} ->
            receive_chunks(Count - 1, Timeout, [Chunk | Acc]);
        {'$resource_stream_complete', _, _} ->
            lists:reverse(Acc)
    after Timeout ->
        lists:reverse(Acc)
    end.
```

### Property-Based Tests (Proper)

```erlang
%% Test URI parsing robustness
prop_uri_parse() ->
    ?FORALL(Uri, uri_gen(),
        begin
            case uri_string:parse(Uri) of
                #{scheme := Scheme, path := Path}
                  when byte_size(Scheme) > 0, byte_size(Path) > 0 ->
                    true;
                _ ->
                    false
            end
        end).

uri_gen() ->
    ?LET({Scheme, Path},
         {oneof([<<"http">>, <<"https">>, <<"file">>, <<"s3">>]),
          binary()},
         <<Scheme/binary, "://", Path/binary>>).
```

---

## Performance Considerations

### Cache Sizing

```erlang
%% Calculate optimal cache size based on available memory
calculate_cache_size() ->
    TotalMemory = erlang:memory(total),
    %% Use 10% of total memory for cache
    MaxCacheSize = TotalMemory div 10,
    %% Estimate average resource size (100 KB)
    AvgResourceSize = 102400,
    %% Calculate max entries
    MaxEntries = MaxCacheSize div AvgResourceSize,
    min(MaxEntries, 10000).  % Cap at 10k entries
```

### Connection Pooling

```erlang
%% HTTP handler uses poolboy for connection reuse
PoolArgs = [
    {name, {local, http_pool}},
    {worker_module, erlmcp_resource_http_handler},
    {size, 10},              % 10 persistent connections
    {max_overflow, 5}        % Allow 5 additional connections
],
poolboy:start_pool(http_pool, PoolArgs).
```

### Streaming Buffer Size

```erlang
%% Optimal chunk size balances throughput and latency
%% - Too small: excessive message passing overhead
%% - Too large: poor backpressure, high memory usage
optimal_chunk_size() ->
    %% 64 KB chunks (tunable based on testing)
    65536.
```

---

## Configuration Reference

### Resource Manager

```erlang
{erlmcp_resource_manager, [
    {default_handler, erlmcp_resource_file_handler},
    {cache_enabled, true},
    {streaming_enabled, true},
    {content_negotiation_enabled, true}
]}.
```

### Resource Cache

```erlang
{erlmcp_resource_cache, [
    {max_entries, 10000},
    {default_ttl, 300000},           % 5 minutes
    {cleanup_interval, 60000},       % 60 seconds
    {max_size_bytes, 1073741824},    % 1 GB
    {cache_dir, "data/resource_cache"}
]}.
```

### HTTP Handler

```erlang
{erlmcp_resource_http_handler, [
    {connect_timeout, 5000},         % milliseconds
    {request_timeout, 30000},        % milliseconds
    {max_redirects, 5},
    {max_body_size, 10485760},       % 10 MB
    {user_agent, <<"erlmcp/3.0">>},
    {pool_size, 10},
    {pool_max_overflow, 5},
    {auth => #{
        type => bearer,
        token => {env_var, "HTTP_AUTH_TOKEN"}
    }},
    {tls_options => [
        {verify, verify_peer},
        {cacertfile, "/etc/ssl/certs/ca.pem"}
    ]}
]}.
```

### S3 Handler (Future)

```erlang
{erlmcp_resource_s3_handler, [
    {access_key_id, {env_var, "AWS_ACCESS_KEY_ID"}},
    {secret_access_key, {env_var, "AWS_SECRET_ACCESS_KEY"}},
    {region, <<"us-east-1">>},
    {bucket, <<"my-bucket">>},
    {connect_timeout, 5000},
    {request_timeout, 30000}
]}.
```

---

## Documentation Updates

### New Documentation Files

1. **`docs/resource-handlers.md`** - Resource handler implementation guide
2. **`docs/http-resources.md`** - HTTP resource usage guide
3. **`docs/streaming-resources.md`** - Streaming API reference
4. **`docs/content-negotiation.md`** - Content negotiation guide
5. **`docs/uri-schemes.md`** - URI scheme reference

### Example Code

**`examples/resource_handlers/http_fetcher.erl`**:

```erlang
%% Fetch HTTP resource with caching
fetch_http_resource(Uri) ->
    case erlmcp_resource_manager:read_resource(Uri) of
        {ok, Content, Metadata} ->
            logger:info("Fetched ~p bytes from ~s",
                        [byte_size(Content), Uri]),
            {ok, Content, Metadata};
        {error, Reason} ->
            logger:error("Failed to fetch ~s: ~p", [Uri, Reason]),
            {error, Reason}
    end.
```

**`examples/resource_handlers/stream_download.erl`**:

```erlang
%% Stream large file to disk
stream_to_disk(Uri, FilePath) ->
    {ok, StreamPid} = erlmcp_resource_manager:read_resource_stream(Uri),
    StreamPid ! {'$resource_stream_request', self(), Uri, 1048576},
    {ok, Fd} = file:open(FilePath, [write, binary]),
    loop_stream(Fd).

loop_stream(Fd) ->
    receive
        {'$resource_stream_chunk', _, Chunk, _} ->
            file:write(Fd, Chunk),
            loop_stream(Fd);
        {'$resource_stream_complete', _, _} ->
            file:close(Fd);
        {'$resource_stream_error', _, Reason} ->
            logger:error("Stream error: ~p", [Reason]),
            file:close(Fd)
    end.
```

---

## Open Questions

1. **S3/GS Handler Priority**: Should S3 and GCP handlers be in v3.0.0 or deferred to v3.1.0?
   - **Recommendation**: Defer to v3.1.0 to focus on HTTP/file streaming in v3.0.0

2. **Cache Persistence**: Should cache persist across restarts?
   - **Recommendation**: No, keep cache in-memory (ETS) for simplicity

3. **Transcoding Scope**: Which content type conversions should be supported?
   - **Recommendation**: Start with JSON ↔ YAML, add more via plugins

4. **Streaming Buffering**: Should stream process buffer data in memory?
   - **Recommendation**: No buffer, strict backpressure only

5. **Error Recovery**: Should HTTP handler retry failed requests?
   - **Recommendation**: Yes, configurable retry with exponential backoff

---

## Success Metrics

### Functional Requirements

- [x] Resource handler behavior defined
- [ ] HTTP handler fetches resources with gun
- [ ] Cache reduces HTTP requests by >50%
- [ ] Streaming handles 100+ MB files without OOM
- [ ] Content negotiation selects correct type
- [ ] Custom URI schemes can be registered

### Performance Requirements

- **HTTP Fetch**: <100ms p50 latency for cached resources
- **Streaming**: >100 MB/s throughput on local network
- **Cache Hit Ratio**: >70% for frequently accessed resources
- **Memory Usage**: <100 MB for 1000 cached resources

### Quality Requirements

- **Test Coverage**: ≥80% for all new modules
- **Dialyzer**: Zero warnings
- **Documentation**: All public APIs documented
- **Examples**: At least 3 working examples

---

## Implementation Timeline

### Week 1: Foundation (Phase 1-2)
- Day 1-2: Resource handler behavior
- Day 3-4: Handler registry
- Day 5: File handler refactoring

### Week 2: HTTP & Cache (Phase 2-3)
- Day 1-3: HTTP handler with gun
- Day 4-5: Cache layer

### Week 3: Streaming & Content (Phase 4-5)
- Day 1-3: Streaming interface
- Day 4-5: Content negotiation

### Week 4: Testing & Documentation
- Day 1-2: Unit tests
- Day 3-4: Integration tests
- Day 5: Documentation

---

## References

- **RFC 3986**: URI Generic Syntax
- **RFC 7231**: Content Negotiation
- **RFC 7230**: Chunked Transfer Coding
- **MCP Spec**: Model Context Protocol 2025-11-25
- **erlmcp Patterns**: `docs/otp-patterns.md`
- **Transport Behavior**: `apps/erlmcp_transports/src/erlmcp_transport_behavior.erl`

---

**End of Design Plan**
