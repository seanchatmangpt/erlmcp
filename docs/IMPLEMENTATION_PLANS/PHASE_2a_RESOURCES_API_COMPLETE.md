# Phase 2a: Complete Resources API Implementation Plan

## Overview

### Current State
The `resources/list` and `resources/read` endpoints are currently stub implementations that do not comply with the MCP 2025-11-25 specification:

1. **resources/list** (line 119-121 in erlmcp_message_handler.erl):
   - Returns only resource URIs (keys) from the server state
   - Does NOT return full resource objects with metadata
   - Does NOT support pagination cursors
   - Does NOT include optional fields (name, description, mimeType)

2. **resources/read** (lines 94-98 in erlmcp_message_handler.erl):
   - Returns hardcoded stub response `<<"resource_content">>`
   - Ignores request parameters completely
   - Does NOT look up actual resource content
   - Does NOT call resource handlers
   - Does NOT handle error cases (not found, access denied)

### Target State
Full MCP 2025-11-25 specification compliance:

1. **resources/list**:
   - Returns complete resource objects with all metadata
   - Supports pagination via cursor parameter
   - Returns nextCursor when more results available
   - Includes uri (required), name, description, mimeType (optional)

2. **resources/read**:
   - Reads actual resource content by URI
   - Calls registered resource handlers
   - Returns proper content objects with uri and mimeType
   - Handles errors: -32001 (not found), -32025 (access denied)

### Effort Estimate
- **Total Time**: 8-12 hours
- **Complexity**: Medium
- **Risk Level**: Low (well-defined interfaces)
- **Dependencies**: None (self-contained changes)

### Impact
- **Functional**: Enables real resource management for MCP clients
- **Compliance**: Closes 2 critical spec gaps
- **Testing**: Requires ~5 new test cases
- **Performance**: Minimal impact (efficient map lookups)

---

## Problem Analysis

### Issue #1: resources/list Returns Stubs

**File**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_message_handler.erl`

**Current Implementation** (lines 119-121):
```erlang
handle_request(<<"resources/list">>, _Params, Id, _TransportId, State) ->
    Resources = maps:keys(State#mcp_server_state.resources),
    {reply, erlmcp_json_rpc:encode_response(Id, #{<<"resources">> => Resources}), State};
```

**Problem**:
- `maps:keys/1` returns only URIs (binary strings)
- MCP spec requires full resource objects
- Spec-compliant response format:
  ```json
  {
    "resources": [
      {
        "uri": "file:///documents/report.pdf",
        "name": "Annual Report",
        "description": "Company annual report 2024",
        "mimeType": "application/pdf"
      }
    ],
    "nextCursor": "optional_cursor_for_pagination"
  }
  ```

**Specification Requirements**:
1. Each resource object MUST include:
   - `uri` (required): binary string
2. Each resource object MAY include:
   - `name` (optional): binary string
   - `description` (optional): binary string
   - `mimeType` (optional): binary string
3. Pagination support:
   - Accept `cursor` parameter (binary string)
   - Return `nextCursor` when more results exist
   - Default page size: 100 resources

---

### Issue #2: resources/read Is Hardcoded Stub

**File**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_message_handler.erl`

**Current Implementation** (lines 94-98):
```erlang
%% @doc Handle resources/read request
-spec handle_read_resource(map(), state()) -> {binary(), state()}.
handle_read_resource(_Params, State) ->
    %% Read resource content
    {<<"resource_content">>, State}.
```

**Problem**:
- Completely ignores input parameters
- Returns hardcoded binary stub
- Does NOT look up resource in state
- Does NOT call resource handler function
- Does NOT validate URI parameter
- Does NOT handle error cases

**Specification Requirements**:
1. Request parameters:
   - `uri` (required): Resource URI to read
2. Response format:
   ```json
   {
     "contents": [
       {
         "uri": "file:///documents/report.pdf",
         "mimeType": "application/pdf",
         "text": "...content..." OR "blob": "base64..."
       }
     ]
   }
   ```
3. Error codes:
   - `-32001` (MCP_ERROR_RESOURCE_NOT_FOUND): Resource doesn't exist
   - `-32025` (MCP_ERROR_RESOURCE_ACCESS_DENIED): Permission denied
   - `-32602` (JSONRPC_INVALID_PARAMS): Missing uri parameter

**Resource Handler Contract**:
- Type: `fun((binary()) -> binary() | #mcp_content{} | {error, Reason})`
- Input: Resource URI (binary)
- Output: Content (binary), structured content record, or error tuple

---

## Solution Architecture

### Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    erlmcp_message_handler                    │
│                                                               │
│  ┌─────────────────────┐      ┌─────────────────────┐      │
│  │  resources/list     │      │  resources/read     │      │
│  │  (line 119-121)     │      │  (lines 94-98)      │      │
│  └──────────┬──────────┘      └──────────┬──────────┘      │
│             │                             │                  │
│             v                             v                  │
│  ┌─────────────────────┐      ┌─────────────────────┐      │
│  │ erlmcp_server:      │      │ erlmcp_server:      │      │
│  │ list_resources/2    │      │ read_resource/2     │      │
│  │ (NEW)               │      │ (NEW)               │      │
│  └──────────┬──────────┘      └──────────┬──────────┘      │
│             │                             │                  │
└─────────────┼─────────────────────────────┼──────────────────┘
              │                             │
              v                             v
     ┌────────────────┐          ┌─────────────────────┐
     │ State lookup   │          │ Handler invocation  │
     │ + pagination   │          │ + error handling    │
     └────────────────┘          └─────────────────────┘
              │                             │
              v                             v
     ┌────────────────┐          ┌─────────────────────┐
     │ encode_resource│          │ normalize_content   │
     │ helper (NEW)   │          │ helper (NEW)        │
     └────────────────┘          └─────────────────────┘
```

### Data Flow: resources/list

```
1. Client sends: {"method": "resources/list", "params": {"cursor": "..."}}
                              ↓
2. erlmcp_message_handler:handle_request/5
   - Extract cursor from params (may be undefined)
                              ↓
3. erlmcp_server:list_resources(State, Cursor)
   - Get resources map from state
   - Sort URIs for stable ordering
   - Apply pagination (skip to cursor position)
   - Take page_size resources (default 100)
   - Calculate nextCursor if more results
                              ↓
4. For each resource, call encode_resource/1
   - Extract #mcp_resource{} record
   - Build map with uri (required)
   - Add name, description, mimeType (if present)
                              ↓
5. Return: {"resources": [...], "nextCursor": "..."}
```

### Data Flow: resources/read

```
1. Client sends: {"method": "resources/read", "params": {"uri": "file://..."}}
                              ↓
2. erlmcp_message_handler:handle_request/5
   - Extract uri from params
   - Validate uri is present (error -32602 if missing)
                              ↓
3. erlmcp_server:read_resource(State, URI)
   - Look up resource in state map by URI
   - Return {error, not_found} if missing
                              ↓
4. Invoke resource handler function
   - Call Handler(URI)
   - Catch errors: permission_denied → {error, access_denied}
                              ↓
5. normalize_resource_content/2
   - If binary: wrap in content object with text field
   - If map: ensure uri and mimeType present
   - Extract mimeType from resource record if needed
                              ↓
6. Return: {"contents": [{"uri": "...", "mimeType": "...", "text": "..."}]}
```

### Error Handling Strategy

| Scenario | Error Code | Message | Action |
|----------|-----------|---------|--------|
| Missing uri parameter | -32602 | "Missing uri parameter" | Return error immediately |
| Resource not found | -32001 | "Resource not found" | Look up fails in state map |
| Access denied | -32025 | "Resource access denied" | Handler throws permission_denied |
| Handler crash | -32603 | "Internal error" | Catch all other exceptions |
| Invalid cursor | -32071 | "Invalid cursor" | Cursor parsing fails |

---

## Implementation: resources/list (Detailed)

### Step 1: Modify erlmcp_message_handler.erl

**Location**: Lines 119-121

**Current Code**:
```erlang
handle_request(<<"resources/list">>, _Params, Id, _TransportId, State) ->
    Resources = maps:keys(State#mcp_server_state.resources),
    {reply, erlmcp_json_rpc:encode_response(Id, #{<<"resources">> => Resources}), State};
```

**New Implementation**:
```erlang
handle_request(<<"resources/list">>, Params, Id, _TransportId, State) ->
    %% Extract optional cursor parameter for pagination
    Cursor = maps:get(<<"cursor">>, Params, undefined),

    %% Call server-side list function with pagination support
    case erlmcp_server:list_resources(State, Cursor) of
        {ok, ResourceObjects, NextCursor} ->
            %% Build result map with resources array
            Result = #{<<"resources">> => ResourceObjects},

            %% Add nextCursor only if more results exist
            Result2 = case NextCursor of
                undefined -> Result;
                _ -> Result#{<<"nextCursor">> => NextCursor}
            end,

            %% Encode and return successful response
            {reply, erlmcp_json_rpc:encode_response(Id, Result2), State};

        {error, invalid_cursor} ->
            %% Invalid cursor format (error code -32071)
            Error = erlmcp_json_rpc:error(?MCP_ERROR_INVALID_CURSOR,
                                          ?MCP_MSG_INVALID_CURSOR),
            {reply, Error, State};

        {error, Reason} ->
            %% Generic error (error code -32603)
            Error = erlmcp_json_rpc:error(?JSONRPC_INTERNAL_ERROR,
                                          format_error(Reason)),
            {reply, Error, State}
    end;
```

**Lines Modified**: 119-121 (3 lines) → 119-143 (25 lines)
**Net Change**: +22 lines

---

### Step 2: Implement erlmcp_server:list_resources/2

**File**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl`
**Location**: After existing resource management functions (around line 400-500)

**Function Signature**:
```erlang
%% @doc List all resources with pagination support.
%% Returns resource objects with full metadata, not just URIs.
%% Pagination uses opaque cursor strings for stable iteration.
%%
%% @param State Server state containing resources map
%% @param Cursor Optional pagination cursor (binary or undefined)
%% @returns {ok, ResourceObjects, NextCursor} | {error, Reason}
%%
%% Example:
%%   {ok, [#{<<"uri">> => <<"file:///a.txt">>, ...}], <<"cursor_100">>}
%%
-spec list_resources(#mcp_server_state{}, binary() | undefined) ->
    {ok, [map()], binary() | undefined} | {error, term()}.
```

**Implementation**:
```erlang
list_resources(State, Cursor) ->
    %% Extract resources map from state
    ResourcesMap = State#mcp_server_state.resources,

    %% Get sorted list of URIs for stable pagination
    %% Sorting ensures consistent order across requests
    AllURIs = lists:sort(maps:keys(ResourcesMap)),

    %% Default page size (configurable via application env)
    PageSize = application:get_env(erlmcp_core, resource_page_size, 100),

    %% Apply pagination logic
    case paginate_resources(AllURIs, Cursor, PageSize) of
        {ok, PageURIs, NextCursor} ->
            %% Convert URIs to full resource objects
            ResourceObjects = [
                encode_resource_from_state(URI, ResourcesMap)
                || URI <- PageURIs
            ],
            {ok, ResourceObjects, NextCursor};

        {error, Reason} ->
            {error, Reason}
    end.
```

**Helper Function: paginate_resources/3**:
```erlang
%% @doc Apply pagination logic to URI list.
%% Cursor format: base64(offset:integer)
%% Example: <<"MTAw">> = base64:encode("100")
%%
-spec paginate_resources([binary()], binary() | undefined, pos_integer()) ->
    {ok, [binary()], binary() | undefined} | {error, invalid_cursor}.
paginate_resources(AllURIs, undefined, PageSize) ->
    %% No cursor: start from beginning
    PageURIs = lists:sublist(AllURIs, PageSize),
    NextCursor = calculate_next_cursor(length(PageURIs), length(AllURIs), PageSize),
    {ok, PageURIs, NextCursor};

paginate_resources(AllURIs, Cursor, PageSize) when is_binary(Cursor) ->
    %% Decode cursor to get offset
    case decode_cursor(Cursor) of
        {ok, Offset} when is_integer(Offset), Offset >= 0 ->
            %% Skip to offset, take PageSize items
            Remaining = lists:nthtail(Offset, AllURIs),
            PageURIs = lists:sublist(Remaining, PageSize),
            NextCursor = calculate_next_cursor(
                Offset + length(PageURIs),
                length(AllURIs),
                PageSize
            ),
            {ok, PageURIs, NextCursor};

        {ok, _InvalidOffset} ->
            {error, invalid_cursor};

        {error, _DecodeError} ->
            {error, invalid_cursor}
    end.

%% @doc Calculate next cursor or undefined if at end
-spec calculate_next_cursor(integer(), integer(), integer()) -> binary() | undefined.
calculate_next_cursor(CurrentOffset, TotalCount, PageSize)
  when CurrentOffset >= TotalCount ->
    %% At end of results
    undefined;
calculate_next_cursor(CurrentOffset, _TotalCount, _PageSize) ->
    %% More results available: encode next offset as cursor
    encode_cursor(CurrentOffset).

%% @doc Encode cursor from integer offset
-spec encode_cursor(integer()) -> binary().
encode_cursor(Offset) ->
    %% Simple base64 encoding of offset as string
    OffsetBin = integer_to_binary(Offset),
    base64:encode(OffsetBin).

%% @doc Decode cursor to integer offset
-spec decode_cursor(binary()) -> {ok, integer()} | {error, term()}.
decode_cursor(Cursor) ->
    try
        Decoded = base64:decode(Cursor),
        Offset = binary_to_integer(Decoded),
        {ok, Offset}
    catch
        _:_ -> {error, invalid_format}
    end.
```

**Helper Function: encode_resource_from_state/2**:
```erlang
%% @doc Encode a resource to JSON-compatible map from state.
%% Extracts #mcp_resource{} record and converts to map.
%%
-spec encode_resource_from_state(binary(), map()) -> map().
encode_resource_from_state(URI, ResourcesMap) ->
    %% State stores: #{URI => {#mcp_resource{}, HandlerFun}}
    {ResourceRecord, _Handler} = maps:get(URI, ResourcesMap),
    encode_resource(ResourceRecord).

%% @doc Encode a resource record to JSON map.
%% Only includes non-undefined optional fields.
%%
-spec encode_resource(#mcp_resource{}) -> map().
encode_resource(#mcp_resource{
    uri = Uri,
    name = Name,
    description = Description,
    mime_type = MimeType
}) ->
    %% Start with required uri field
    Base = #{<<"uri">> => Uri},

    %% Add optional fields only if present
    Base1 = maybe_add_field(Base, <<"name">>, Name),
    Base2 = maybe_add_field(Base1, <<"description">>, Description),
    maybe_add_field(Base2, <<"mimeType">>, MimeType).

%% @doc Add field to map only if value is not undefined
-spec maybe_add_field(map(), binary(), term() | undefined) -> map().
maybe_add_field(Map, _Key, undefined) ->
    Map;
maybe_add_field(Map, Key, Value) ->
    Map#{Key => Value}.
```

**Lines Added**: ~120 lines
**Location**: After line 400 in erlmcp_server.erl

---

## Implementation: resources/read (Detailed)

### Step 1: Modify erlmcp_message_handler.erl

**Location**: Lines 94-98

**Current Code**:
```erlang
%% @doc Handle resources/read request
-spec handle_read_resource(map(), state()) -> {binary(), state()}.
handle_read_resource(_Params, State) ->
    %% Read resource content
    {<<"resource_content">>, State}.
```

**New Implementation**:
```erlang
%% Note: This function signature is not used by current routing!
%% The actual routing happens in handle_request/5 which we need to add.

%% Add this new handle_request clause for resources/read:
handle_request(<<"resources/read">>, Params, Id, _TransportId, State) ->
    %% Extract required uri parameter
    case maps:get(<<"uri">>, Params, undefined) of
        undefined ->
            %% Error -32602: Invalid params (missing uri)
            Error = erlmcp_json_rpc:error(?JSONRPC_INVALID_PARAMS,
                                          <<"Missing uri parameter">>),
            {reply, Error, State};

        URI when is_binary(URI) ->
            %% Call server-side read function
            case erlmcp_server:read_resource(State, URI) of
                {ok, ContentObject} ->
                    %% Wrap in contents array per MCP spec
                    Result = #{<<"contents">> => [ContentObject]},
                    {reply, erlmcp_json_rpc:encode_response(Id, Result), State};

                {error, not_found} ->
                    %% Error -32001: Resource not found
                    Error = erlmcp_json_rpc:error(?MCP_ERROR_RESOURCE_NOT_FOUND,
                                                  ?MCP_MSG_RESOURCE_NOT_FOUND),
                    {reply, Error, State};

                {error, access_denied} ->
                    %% Error -32025: Resource access denied
                    Error = erlmcp_json_rpc:error(?MCP_ERROR_RESOURCE_ACCESS_DENIED,
                                                  ?MCP_MSG_RESOURCE_ACCESS_DENIED),
                    {reply, Error, State};

                {error, Reason} ->
                    %% Generic error -32603: Internal error
                    Error = erlmcp_json_rpc:error(?JSONRPC_INTERNAL_ERROR,
                                                  format_error(Reason)),
                    {reply, Error, State}
            end;

        _InvalidURI ->
            %% Error -32602: Invalid params (uri not a string)
            Error = erlmcp_json_rpc:error(?JSONRPC_INVALID_PARAMS,
                                          <<"Invalid uri parameter type">>),
            {reply, Error, State}
    end;
```

**Location**: Add this as a new clause in handle_request/5, after line 127
**Lines Added**: ~45 lines

**Note**: The old handle_read_resource/2 function (lines 94-98) can be REMOVED as it's not used.

---

### Step 2: Implement erlmcp_server:read_resource/2

**File**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl`
**Location**: After list_resources functions (around line 520-600)

**Function Signature**:
```erlang
%% @doc Read resource content by URI.
%% Looks up resource in state, invokes handler, normalizes output.
%%
%% @param State Server state containing resources map
%% @param URI Resource URI to read
%% @returns {ok, ContentObject} | {error, Reason}
%%
%% Error reasons:
%%   - not_found: Resource URI not registered
%%   - access_denied: Handler threw permission_denied
%%   - Other: Handler crashed or returned error
%%
-spec read_resource(#mcp_server_state{}, binary()) ->
    {ok, map()} | {error, not_found | access_denied | term()}.
```

**Implementation**:
```erlang
read_resource(State, URI) when is_binary(URI) ->
    %% Look up resource in state map
    ResourcesMap = State#mcp_server_state.resources,

    case maps:get(URI, ResourcesMap, undefined) of
        undefined ->
            %% Resource not registered
            {error, not_found};

        {ResourceRecord, Handler} ->
            %% Resource found: invoke handler
            try
                %% Call handler function with URI
                RawContent = Handler(URI),

                %% Normalize handler output to content object
                ContentObject = normalize_resource_content(RawContent, ResourceRecord),

                {ok, ContentObject}
            catch
                error:permission_denied ->
                    %% Handler explicitly denied access
                    {error, access_denied};

                error:Reason:Stacktrace ->
                    %% Handler crashed
                    logger:error("Resource handler crashed for URI ~s: ~p~n~p",
                                 [URI, Reason, Stacktrace]),
                    {error, {handler_crash, Reason}};

                throw:{error, Reason} ->
                    %% Handler returned error tuple via throw
                    {error, Reason};

                Class:Reason:Stacktrace ->
                    %% Other exception
                    logger:error("Resource handler exception for URI ~s: ~p:~p~n~p",
                                 [URI, Class, Reason, Stacktrace]),
                    {error, {handler_exception, Reason}}
            end
    end.
```

**Helper Function: normalize_resource_content/2**:
```erlang
%% @doc Normalize resource handler output to MCP content object.
%% Handlers can return various formats:
%%   - Binary: plain text content
%%   - #mcp_content{}: structured content record
%%   - Map: pre-formatted content object
%%   - {error, Reason}: error tuple (thrown as exception)
%%
%% All outputs normalized to map with required fields:
%%   - uri (binary): resource URI
%%   - mimeType (binary): content MIME type
%%   - text (binary) OR blob (binary): content data
%%
-spec normalize_resource_content(term(), #mcp_resource{}) -> map().
normalize_resource_content(Content, Resource) when is_binary(Content) ->
    %% Plain binary text: wrap in content object
    #{
        <<"uri">> => Resource#mcp_resource.uri,
        <<"mimeType">> => get_mime_type(Resource),
        <<"text">> => Content
    };

normalize_resource_content(#mcp_content{} = Content, Resource) ->
    %% Structured content record: convert to map
    BaseMap = #{
        <<"uri">> => Resource#mcp_resource.uri,
        <<"mimeType">> => get_content_mime_type(Content, Resource)
    },

    %% Add content data (text or blob)
    case Content#mcp_content.text of
        undefined ->
            %% Binary blob content
            case Content#mcp_content.data of
                undefined ->
                    %% No content data!
                    BaseMap#{<<"text">> => <<>>};
                BlobData ->
                    BaseMap#{<<"blob">> => base64:encode(BlobData)}
            end;
        TextData ->
            %% Text content
            BaseMap#{<<"text">> => TextData}
    end;

normalize_resource_content(ContentMap, Resource) when is_map(ContentMap) ->
    %% Pre-formatted map: ensure required fields present
    BaseMap = #{
        <<"uri">> => maps:get(<<"uri">>, ContentMap, Resource#mcp_resource.uri),
        <<"mimeType">> => maps:get(<<"mimeType">>, ContentMap, get_mime_type(Resource))
    },

    %% Merge with original map (preserves text/blob and other fields)
    maps:merge(BaseMap, ContentMap);

normalize_resource_content({error, Reason}, _Resource) ->
    %% Handler returned error tuple: throw to be caught above
    throw({error, Reason});

normalize_resource_content(Other, Resource) ->
    %% Unknown format: log warning and return as text
    logger:warning("Resource handler returned unexpected format for ~s: ~p",
                   [Resource#mcp_resource.uri, Other]),
    #{
        <<"uri">> => Resource#mcp_resource.uri,
        <<"mimeType">> => get_mime_type(Resource),
        <<"text">> => iolist_to_binary(io_lib:format("~p", [Other]))
    }.

%% @doc Get MIME type from resource record or default
-spec get_mime_type(#mcp_resource{}) -> binary().
get_mime_type(#mcp_resource{mime_type = MimeType}) when is_binary(MimeType) ->
    MimeType;
get_mime_type(_) ->
    <<"text/plain">>.  % Default MIME type

%% @doc Get MIME type from content record or resource
-spec get_content_mime_type(#mcp_content{}, #mcp_resource{}) -> binary().
get_content_mime_type(#mcp_content{mime_type = MT}, _Resource) when is_binary(MT) ->
    MT;
get_content_mime_type(_Content, Resource) ->
    get_mime_type(Resource).
```

**Lines Added**: ~110 lines
**Location**: After list_resources functions in erlmcp_server.erl

---

## Step 3: Verify Error Code Constants

**File**: `/home/user/erlmcp/apps/erlmcp_core/include/erlmcp.hrl`

**Check for Required Constants** (already present):
```erlang
%% Line 55: Resource not found
-define(MCP_ERROR_RESOURCE_NOT_FOUND, -32001).

%% Line 84: Resource access denied
-define(MCP_ERROR_RESOURCE_ACCESS_DENIED, -32025).

%% Line 139: Invalid cursor
-define(MCP_ERROR_INVALID_CURSOR, -32071).

%% Line 12: Invalid params
-define(JSONRPC_INVALID_PARAMS, -32602).

%% Line 13: Internal error
-define(JSONRPC_INTERNAL_ERROR, -32603).
```

**Check for Required Messages** (already present):
```erlang
%% Line 324: Resource not found message
-define(MCP_MSG_RESOURCE_NOT_FOUND, <<"Resource not found">>).

%% Line 352: Resource access denied message
-define(MCP_MSG_RESOURCE_ACCESS_DENIED, <<"Resource access denied">>).

%% Line 407: Invalid cursor message
-define(MCP_MSG_INVALID_CURSOR, <<"Invalid cursor">>).
```

**Result**: All required error codes and messages are already defined. No changes needed to erlmcp.hrl.

---

## Step 4: Update Client-Side Parsing

### File: erlmcp_client.erl

**Current list_resources/1** (lines 116-118):
```erlang
-spec list_resources(client()) -> {ok, [map()]} | {error, term()}.
list_resources(Client) ->
    gen_server:call(Client, list_resources, 2000).
```

**Analysis**: The client-side function signature is correct. It already returns `{ok, [map()]}` which will contain the full resource objects. The internal gen_server:call handling should work correctly with the new response format.

**Verification Needed**: Check handle_response in erlmcp_client.erl to ensure it properly extracts the `resources` array.

**Current read_resource/2** (lines 120-122):
```erlang
-spec read_resource(client(), binary()) -> {ok, map()} | {error, term()}.
read_resource(Client, Uri) when is_binary(Uri) ->
    gen_server:call(Client, {read_resource, Uri}).
```

**Analysis**: The function signature looks correct. Need to verify the response handling extracts the first element from the `contents` array.

### Update Response Handling (if needed)

**Location**: erlmcp_client.erl, handle_response/3 function (around lines 696-734)

The current implementation should handle these correctly, but we should verify the response parsing logic explicitly handles:

1. **resources/list response**:
   ```erlang
   %% In handle_response for list_resources:
   case maps:get(<<"resources">>, Result, undefined) of
       undefined -> {error, invalid_response};
       Resources when is_list(Resources) -> {ok, Resources}
   end
   ```

2. **resources/read response**:
   ```erlang
   %% In handle_response for read_resource:
   case maps:get(<<"contents">>, Result, undefined) of
       undefined -> {error, invalid_response};
       [FirstContent | _] -> {ok, FirstContent};
       [] -> {error, empty_contents}
   end
   ```

**Action**: Review handle_response/3 and update if needed. Estimate: 10 lines changed if needed.

---

## Step 5: Test Implementation

### File: erlmcp_resource_integration_tests.erl

**Location**: `/home/user/erlmcp/apps/erlmcp_core/test/erlmcp_resource_integration_tests.erl`

### Test 1: resources/list Returns Full Objects

```erlang
%% @doc Test that resources/list returns full resource objects, not just URIs
test_resources_list_returns_objects() ->
    %% Start server
    Caps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{}
    },
    {ok, Server} = erlmcp_server:start_link(test_server, Caps),

    %% Add test resources
    Handler1 = fun(_) -> <<"content1">> end,
    Handler2 = fun(_) -> <<"content2">> end,

    ok = erlmcp_server:add_resource(Server, <<"file:///doc1.txt">>, Handler1),
    ok = erlmcp_server:add_resource(Server, <<"file:///doc2.txt">>, Handler2),

    %% Get server state
    State = sys:get_state(Server),

    %% Call list_resources
    {ok, Resources, _NextCursor} = erlmcp_server:list_resources(State, undefined),

    %% Verify we got full objects, not just URIs
    ?assertEqual(2, length(Resources)),

    %% Check first resource is a map with uri field
    [Resource1 | _] = Resources,
    ?assert(is_map(Resource1)),
    ?assert(maps:is_key(<<"uri">>, Resource1)),

    %% Verify all resources have uri field
    URIs = [maps:get(<<"uri">>, R) || R <- Resources],
    ?assertEqual(lists:sort([<<"file:///doc1.txt">>, <<"file:///doc2.txt">>]),
                 lists:sort(URIs)),

    %% Clean up
    erlmcp_server:stop(Server).
```

### Test 2: resources/list Pagination Support

```erlang
%% @doc Test pagination with cursor parameter
test_resources_list_pagination() ->
    %% Start server
    Caps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{}
    },
    {ok, Server} = erlmcp_server:start_link(test_server, Caps),

    %% Add 250 resources (more than 1 page at default size 100)
    Handler = fun(_) -> <<"content">> end,
    lists:foreach(fun(N) ->
        URI = iolist_to_binary(io_lib:format("file:///doc~w.txt", [N])),
        ok = erlmcp_server:add_resource(Server, URI, Handler)
    end, lists:seq(1, 250)),

    State = sys:get_state(Server),

    %% Page 1: No cursor
    {ok, Page1, Cursor1} = erlmcp_server:list_resources(State, undefined),
    ?assertEqual(100, length(Page1)),
    ?assert(is_binary(Cursor1)),  % NextCursor should be present

    %% Page 2: Use cursor from page 1
    {ok, Page2, Cursor2} = erlmcp_server:list_resources(State, Cursor1),
    ?assertEqual(100, length(Page2)),
    ?assert(is_binary(Cursor2)),  % NextCursor should be present

    %% Page 3: Use cursor from page 2 (final page)
    {ok, Page3, Cursor3} = erlmcp_server:list_resources(State, Cursor2),
    ?assertEqual(50, length(Page3)),
    ?assertEqual(undefined, Cursor3),  % No more pages

    %% Verify no overlap between pages
    URIs1 = [maps:get(<<"uri">>, R) || R <- Page1],
    URIs2 = [maps:get(<<"uri">>, R) || R <- Page2],
    URIs3 = [maps:get(<<"uri">>, R) || R <- Page3],

    AllURIs = URIs1 ++ URIs2 ++ URIs3,
    ?assertEqual(250, length(AllURIs)),
    ?assertEqual(250, length(lists:usort(AllURIs))),  % No duplicates

    erlmcp_server:stop(Server).
```

### Test 3: resources/read Returns Content

```erlang
%% @doc Test that resources/read returns actual content
test_resources_read_returns_content() ->
    %% Start server
    Caps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{}
    },
    {ok, Server} = erlmcp_server:start_link(test_server, Caps),

    %% Add resource with handler that returns content
    ExpectedContent = <<"This is the file content">>,
    Handler = fun(<<"file:///test.txt">>) -> ExpectedContent end,

    ok = erlmcp_server:add_resource(Server, <<"file:///test.txt">>, Handler),

    State = sys:get_state(Server),

    %% Read resource
    {ok, ContentObject} = erlmcp_server:read_resource(State, <<"file:///test.txt">>),

    %% Verify content object structure
    ?assert(is_map(ContentObject)),
    ?assertEqual(<<"file:///test.txt">>, maps:get(<<"uri">>, ContentObject)),
    ?assert(maps:is_key(<<"mimeType">>, ContentObject)),
    ?assertEqual(ExpectedContent, maps:get(<<"text">>, ContentObject)),

    erlmcp_server:stop(Server).
```

### Test 4: resources/read Error Handling - Not Found

```erlang
%% @doc Test resources/read with non-existent URI
test_resources_read_not_found() ->
    %% Start server
    Caps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{}
    },
    {ok, Server} = erlmcp_server:start_link(test_server, Caps),

    State = sys:get_state(Server),

    %% Try to read non-existent resource
    Result = erlmcp_server:read_resource(State, <<"file:///missing.txt">>),

    %% Verify error response
    ?assertEqual({error, not_found}, Result),

    erlmcp_server:stop(Server).
```

### Test 5: resources/read Error Handling - Access Denied

```erlang
%% @doc Test resources/read with permission denied
test_resources_read_access_denied() ->
    %% Start server
    Caps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{}
    },
    {ok, Server} = erlmcp_server:start_link(test_server, Caps),

    %% Add resource with handler that denies access
    Handler = fun(_) -> error(permission_denied) end,

    ok = erlmcp_server:add_resource(Server, <<"file:///secret.txt">>, Handler),

    State = sys:get_state(Server),

    %% Try to read protected resource
    Result = erlmcp_server:read_resource(State, <<"file:///secret.txt">>),

    %% Verify access denied error
    ?assertEqual({error, access_denied}, Result),

    erlmcp_server:stop(Server).
```

### Test 6: End-to-End JSON-RPC Test

```erlang
%% @doc Test full JSON-RPC message flow for resources/list
test_resources_list_jsonrpc_e2e() ->
    %% Start server
    Caps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{}
    },
    {ok, Server} = erlmcp_server:start_link(test_server, Caps),

    %% Add resources
    Handler = fun(_) -> <<"content">> end,
    ok = erlmcp_server:add_resource(Server, <<"file:///a.txt">>, Handler),
    ok = erlmcp_server:add_resource(Server, <<"file:///b.txt">>, Handler),

    State = sys:get_state(Server),

    %% Simulate JSON-RPC request
    RequestData = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"resources/list">>,
        <<"params">> => #{}
    }),

    %% Process message through handler
    {reply, ResponseData, _NewState} =
        erlmcp_message_handler:process_message(<<"test_transport">>, RequestData, State),

    %% Decode response
    Response = jsx:decode(ResponseData, [return_maps]),

    %% Verify response structure
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
    ?assertEqual(1, maps:get(<<"id">>, Response)),

    Result = maps:get(<<"result">>, Response),
    ?assert(is_map(Result)),

    Resources = maps:get(<<"resources">>, Result),
    ?assert(is_list(Resources)),
    ?assertEqual(2, length(Resources)),

    %% Verify each resource is an object with uri
    lists:foreach(fun(R) ->
        ?assert(is_map(R)),
        ?assert(maps:is_key(<<"uri">>, R))
    end, Resources),

    erlmcp_server:stop(Server).
```

**Lines Added**: ~200 lines of test code
**File**: Append to erlmcp_resource_integration_tests.erl

---

## Verification Checklist

### Functional Requirements

- [ ] **resources/list returns full resource objects**
  - Test: `test_resources_list_returns_objects`
  - Verify: Each object has `uri` field (required)
  - Verify: Optional fields (`name`, `description`, `mimeType`) included when present

- [ ] **resources/list supports pagination**
  - Test: `test_resources_list_pagination`
  - Verify: Returns `nextCursor` when more results exist
  - Verify: Cursor works for subsequent requests
  - Verify: `nextCursor` is `undefined` on last page
  - Verify: No duplicate resources across pages

- [ ] **resources/read returns actual content**
  - Test: `test_resources_read_returns_content`
  - Verify: Calls resource handler with URI
  - Verify: Returns content object with `uri`, `mimeType`, `text`/`blob`
  - Verify: Handles binary, record, and map handler outputs

- [ ] **resources/read handles errors correctly**
  - Test: `test_resources_read_not_found`
  - Verify: Returns `{error, not_found}` for missing URI
  - Test: `test_resources_read_access_denied`
  - Verify: Returns `{error, access_denied}` when handler throws permission_denied
  - Verify: Error codes map correctly (-32001, -32025, -32602, -32603)

- [ ] **Client-side parsing updated**
  - Manual verification: erlmcp_client.erl handles response arrays
  - Test: Client integration tests pass

- [ ] **End-to-end JSON-RPC flow works**
  - Test: `test_resources_list_jsonrpc_e2e`
  - Verify: Full message encoding/decoding works
  - Verify: Response matches MCP spec format

### Code Quality

- [ ] **Compilation succeeds**
  - Command: `TERM=dumb rebar3 compile`
  - Expected: 0 errors, 0 warnings

- [ ] **All unit tests pass**
  - Command: `rebar3 eunit --module=erlmcp_resource_integration_tests`
  - Expected: 6/6 tests pass

- [ ] **Type checking passes**
  - Command: `rebar3 dialyzer`
  - Expected: 0 type errors

- [ ] **Cross-reference check passes**
  - Command: `rebar3 xref`
  - Expected: 0 undefined function calls

- [ ] **Code formatting correct**
  - Command: `rebar3 format --verify`
  - Expected: No formatting issues

### Performance

- [ ] **Pagination performance acceptable**
  - Measure: Time to list 10,000 resources with page size 100
  - Target: <10ms per page
  - Method: Benchmark with `timer:tc/1`

- [ ] **Resource read latency acceptable**
  - Measure: Time to read single resource
  - Target: <1ms (excluding handler execution time)
  - Method: Benchmark with `timer:tc/1`

### Documentation

- [ ] **Function specs complete**
  - All new functions have `-spec` declarations
  - Types match implementation

- [ ] **Comments clear**
  - Each function has `@doc` comment
  - Complex logic explained inline

- [ ] **Error messages descriptive**
  - All error returns have clear messages
  - Error codes match MCP spec

---

## Testing Commands

### Compilation
```bash
cd /home/user/erlmcp
TERM=dumb rebar3 compile
```

**Expected Output**:
```
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling erlmcp_core
===> Compiling erlmcp_transports
===> Compiling erlmcp_observability
===> Compiling erlmcp_validation
```

**Success Criteria**: 0 compilation errors, 0 warnings

---

### Unit Tests
```bash
# Run resource integration tests
rebar3 eunit --module=erlmcp_resource_integration_tests

# Run all core tests (includes resources)
rebar3 eunit --app=erlmcp_core

# Run with verbose output for debugging
rebar3 eunit --module=erlmcp_resource_integration_tests --verbose
```

**Expected Output**:
```
erlmcp_resource_integration_tests:
  test_resources_list_returns_objects............... [ok]
  test_resources_list_pagination.................... [ok]
  test_resources_read_returns_content............... [ok]
  test_resources_read_not_found..................... [ok]
  test_resources_read_access_denied................. [ok]
  test_resources_list_jsonrpc_e2e................... [ok]

All 6 tests passed.
```

**Success Criteria**: 6/6 tests pass, 0 failures

---

### Type Checking
```bash
rebar3 dialyzer
```

**Expected Output**:
```
===> Verifying dependencies...
===> Analyzing applications...
===> Running dialyzer...
done (passed successfully)
```

**Success Criteria**: 0 type warnings

---

### Cross-Reference Check
```bash
rebar3 xref
```

**Expected Output**:
```
===> Verifying dependencies...
===> Analyzing applications...
===> Running cross reference analysis...
```

**Success Criteria**: 0 undefined function calls

---

### Code Formatting
```bash
rebar3 format --verify
```

**Expected Output**:
```
===> Verifying dependencies...
===> Formatting verification passed
```

**Success Criteria**: No formatting issues

---

### Manual Integration Test
```bash
# Start Erlang shell with application loaded
make console

# In Erlang shell:
1> {ok, Server} = erlmcp_server:start_link(test, #{capabilities => #mcp_server_capabilities{}}).
2> Handler = fun(_) -> <<"Hello World">> end.
3> erlmcp_server:add_resource(Server, <<"file:///test.txt">>, Handler).
4> State = sys:get_state(Server).
5> erlmcp_server:list_resources(State, undefined).
% Expected: {ok, [#{<<"uri">> := <<"file:///test.txt">>}], undefined}

6> erlmcp_server:read_resource(State, <<"file:///test.txt">>).
% Expected: {ok, #{<<"uri">> := ..., <<"mimeType">> := ..., <<"text">> := <<"Hello World">>}}
```

---

## Timeline & Effort Breakdown

### Phase 1: resources/list Implementation (4-5 hours)
- **1.5 hours**: Implement list_resources/2 and pagination helpers
  - Write paginate_resources/3
  - Write encode_resource/1 and helpers
  - Write cursor encoding/decoding
- **1 hour**: Modify erlmcp_message_handler.erl
  - Update handle_request clause
  - Add error handling
- **1.5 hours**: Write tests
  - test_resources_list_returns_objects
  - test_resources_list_pagination
- **0.5 hours**: Debug and verify
  - Run tests
  - Fix any issues
- **0.5 hours**: Code review and documentation

### Phase 2: resources/read Implementation (4-5 hours)
- **1.5 hours**: Implement read_resource/2
  - Write main function
  - Write normalize_resource_content/2
  - Add error handling with try/catch
- **1 hour**: Modify erlmcp_message_handler.erl
  - Add handle_request clause
  - Add parameter validation
- **1.5 hours**: Write tests
  - test_resources_read_returns_content
  - test_resources_read_not_found
  - test_resources_read_access_denied
- **0.5 hours**: Debug and verify
  - Run tests
  - Fix any issues
- **0.5 hours**: Code review and documentation

### Phase 3: Integration & Polish (1-2 hours)
- **0.5 hours**: End-to-end JSON-RPC test
  - Write test_resources_list_jsonrpc_e2e
- **0.5 hours**: Client-side verification
  - Review erlmcp_client response handling
  - Make adjustments if needed
- **0.5 hours**: Full test suite run
  - rebar3 compile
  - rebar3 eunit
  - rebar3 dialyzer
  - rebar3 xref
- **0.5 hours**: Performance benchmarking (optional)
  - Measure pagination performance
  - Measure read latency

### Total Time: 9-12 hours

**Breakdown by Activity**:
- Implementation: 5 hours (55%)
- Testing: 3 hours (33%)
- Debug/Verification: 1 hour (11%)

**Breakdown by Component**:
- erlmcp_server.erl: 3 hours (230 lines)
- erlmcp_message_handler.erl: 2 hours (70 lines)
- Tests: 3 hours (200 lines)
- Integration & Polish: 2 hours

---

## Files Modified Summary

### Core Implementation Files

| File | Lines Before | Lines After | Change | Description |
|------|-------------|-------------|--------|-------------|
| `apps/erlmcp_core/src/erlmcp_message_handler.erl` | 139 | 209 | +70 | Add resources/list and resources/read handlers |
| `apps/erlmcp_core/src/erlmcp_server.erl` | ~1500 | ~1730 | +230 | Add list_resources/2, read_resource/2, helpers |
| `apps/erlmcp_core/src/erlmcp_client.erl` | 912 | ~922 | +10 | Update response parsing (if needed) |
| `apps/erlmcp_core/include/erlmcp.hrl` | 1223 | 1223 | 0 | No changes (constants already exist) |

### Test Files

| File | Lines Before | Lines After | Change | Description |
|------|-------------|-------------|--------|-------------|
| `apps/erlmcp_core/test/erlmcp_resource_integration_tests.erl` | ~100 | ~300 | +200 | Add 6 new integration tests |

### Total Changes
- **Production Code**: 310 lines added
- **Test Code**: 200 lines added
- **Total**: 510 lines added
- **Files Modified**: 5 files
- **Files Created**: 0 files (all modifications)

---

## Risk Assessment & Mitigation

### Risk 1: Pagination Cursor Security
**Risk**: Malicious cursors could cause crashes or DOS
**Severity**: Medium
**Mitigation**:
- Base64 decode wrapped in try/catch
- Validate decoded offset is non-negative integer
- Limit maximum offset to prevent memory exhaustion
- Return error -32071 for invalid cursors

### Risk 2: Handler Execution Safety
**Risk**: Resource handlers could crash server process
**Severity**: High
**Mitigation**:
- All handler calls wrapped in try/catch
- Log crashes with full stacktrace
- Return generic error to client
- Preserve server state on handler failure

### Risk 3: Large Resource Lists
**Risk**: Servers with 100,000+ resources could have slow pagination
**Severity**: Low
**Mitigation**:
- Use maps:keys/1 (O(N)) only once per request
- Sort URIs once, reuse for pagination
- Configurable page size (default 100)
- Consider caching sorted list in state (future optimization)

### Risk 4: Breaking Existing Clients
**Risk**: Response format change could break existing code
**Severity**: Medium
**Mitigation**:
- Maintain backward compatibility in response structure
- Resources array contains maps (extensible)
- Old clients expecting strings will fail gracefully
- Document migration path for existing integrations

### Risk 5: Test Coverage Gaps
**Risk**: Edge cases not covered by tests
**Severity**: Low
**Mitigation**:
- Test with 0, 1, 100, 1000 resources
- Test all error paths explicitly
- Test cursor edge cases (empty, invalid, expired)
- Manual testing with real MCP clients

---

## Success Criteria

### Functional Success
1. ✅ **resources/list returns full objects** with uri, name, description, mimeType
2. ✅ **Pagination works correctly** with cursor parameter and nextCursor response
3. ✅ **resources/read returns actual content** by invoking handlers
4. ✅ **Error handling complete** with correct error codes for all failure cases
5. ✅ **Client compatibility maintained** with updated response parsing

### Quality Success
1. ✅ **Zero compilation errors** (TERM=dumb rebar3 compile)
2. ✅ **100% test pass rate** (6/6 tests in erlmcp_resource_integration_tests)
3. ✅ **Zero type warnings** (rebar3 dialyzer)
4. ✅ **Zero xref issues** (rebar3 xref)
5. ✅ **Code formatted correctly** (rebar3 format --verify)

### Performance Success
1. ✅ **Pagination latency <10ms** per page (100 resources)
2. ✅ **Read latency <1ms** (excluding handler execution)
3. ✅ **Memory usage stable** (no leaks in long-running pagination)

### Documentation Success
1. ✅ **All functions have specs** with correct types
2. ✅ **Error codes documented** in function comments
3. ✅ **Implementation guide complete** (this document)

---

## Next Steps After Completion

### Immediate (Week 1)
1. **Code Review**: Submit PR for team review
2. **Manual Testing**: Test with real MCP clients (Claude Desktop, etc.)
3. **Performance Profiling**: Run benchmarks on production-scale data
4. **Documentation Update**: Update API docs with new response formats

### Short Term (Week 2-4)
1. **Phase 2b: Tools API**: Apply same patterns to tools/list and tools/call
2. **Phase 2c: Prompts API**: Apply same patterns to prompts/list and prompts/get
3. **Cursor Optimization**: Consider caching sorted lists for large datasets
4. **Rate Limiting**: Add rate limits to prevent pagination abuse

### Long Term (Month 2-3)
1. **Advanced Pagination**: Support filters, sorting, search in resources/list
2. **Batch Operations**: Support reading multiple resources in single request
3. **Resource Streaming**: Stream large resource content instead of loading fully
4. **Metrics & Monitoring**: Add OpenTelemetry spans for resource operations

---

## Appendix A: MCP Specification Reference

### resources/list Specification (MCP 2025-11-25)

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "resources/list",
  "params": {
    "cursor": "optional_pagination_cursor"
  }
}
```

**Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "resources": [
      {
        "uri": "file:///path/to/resource",
        "name": "Resource Name",
        "description": "Optional description",
        "mimeType": "text/plain"
      }
    ],
    "nextCursor": "optional_next_cursor"
  }
}
```

**Fields**:
- `resources` (array, required): Array of resource objects
- `resources[].uri` (string, required): Unique resource identifier
- `resources[].name` (string, optional): Human-readable name
- `resources[].description` (string, optional): Description
- `resources[].mimeType` (string, optional): MIME type
- `nextCursor` (string, optional): Cursor for next page

---

### resources/read Specification (MCP 2025-11-25)

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "resources/read",
  "params": {
    "uri": "file:///path/to/resource"
  }
}
```

**Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": {
    "contents": [
      {
        "uri": "file:///path/to/resource",
        "mimeType": "text/plain",
        "text": "Resource content as text"
      }
    ]
  }
}
```

**OR (for binary content)**:
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": {
    "contents": [
      {
        "uri": "file:///path/to/resource",
        "mimeType": "application/octet-stream",
        "blob": "base64encodedcontent"
      }
    ]
  }
}
```

**Fields**:
- `contents` (array, required): Array of content objects (usually 1 element)
- `contents[].uri` (string, required): Resource URI
- `contents[].mimeType` (string, required): MIME type
- `contents[].text` (string, conditional): Text content (if text-based)
- `contents[].blob` (string, conditional): Base64 binary content (if binary)

**Error Codes**:
- `-32001`: Resource not found
- `-32025`: Resource access denied
- `-32602`: Invalid params (missing uri)
- `-32603`: Internal error (handler crash)

---

## Appendix B: Code References

### Key Data Structures

**Resource Record** (erlmcp.hrl:807-818):
```erlang
-record(mcp_resource, {
    uri :: binary(),
    name :: binary(),
    description :: binary() | undefined,
    mime_type :: binary() | undefined,
    metadata :: map() | undefined,
    audience :: binary() | undefined,
    priority :: integer() | undefined,
    last_modified :: integer() | undefined,
    annotations :: map() | undefined,
    size :: integer() | undefined
}).
```

**Content Record** (erlmcp.hrl:798-805):
```erlang
-record(mcp_content, {
    type :: binary(),
    text :: binary() | undefined,
    data :: binary() | undefined,
    mime_type :: binary() | undefined,
    annotations = [] :: [#mcp_annotation{}],
    resource_link = undefined :: #mcp_resource_link{} | undefined
}).
```

**Server State** (erlmcp_server.erl:47-68):
```erlang
-record(state, {
    server_id :: server_id(),
    phase = ?MCP_PHASE_INITIALIZATION :: mcp_server_phase(),
    resources = #{} :: #{binary() => {#mcp_resource{}, resource_handler()}},
    % ... other fields
}).
```

### Relevant Functions

**Resource Handler Type** (erlmcp.hrl:913):
```erlang
-type resource_handler() :: fun((binary()) -> binary() | #mcp_content{}).
```

**JSON-RPC Encoding** (erlmcp_json_rpc.erl):
```erlang
-spec encode_response(json_rpc_id(), term()) -> binary().
-spec error(integer(), binary()) -> binary().
```

---

## Appendix C: Testing Strategy

### Test Pyramid

```
     /\
    /  \     6 End-to-End Tests
   /    \    (JSON-RPC message flow)
  /------\
 /        \
/  Unit    \ 20 Unit Tests
\  Tests   / (Function-level)
 \        /
  \------/
   \    /    5 Integration Tests
    \  /     (Multi-function)
     \/
```

### Test Coverage Goals

| Component | Target Coverage | Strategy |
|-----------|----------------|----------|
| list_resources/2 | 100% | Test all branches (cursor/no cursor, has more/no more) |
| read_resource/2 | 100% | Test all error paths + success |
| paginate_resources/3 | 100% | Test edge cases (empty, 1 item, exact page, overflow) |
| encode_resource/1 | 100% | Test with all optional fields present/absent |
| normalize_resource_content/2 | 100% | Test all input types (binary, record, map, error) |

### Property-Based Testing (Future)

Consider adding PropEr tests for:
1. **Pagination invariants**:
   - All items returned exactly once across all pages
   - Order stable regardless of cursor
   - No duplicates across pages
2. **Content normalization**:
   - Output always has required fields
   - Input/output equivalence for round-trips

---

## Appendix D: Performance Benchmarks

### Benchmark 1: Pagination Performance

```erlang
benchmark_pagination(N) ->
    %% Setup: Create N resources
    Resources = lists:foldl(fun(I, Acc) ->
        URI = iolist_to_binary(io_lib:format("file:///~w.txt", [I])),
        Acc#{URI => {#mcp_resource{uri = URI, name = <<"Test">>}, fun(_) -> <<"ok">> end}}
    end, #{}, lists:seq(1, N)),

    State = #mcp_server_state{resources = Resources},

    %% Benchmark: List all resources with pagination
    {Time, _Result} = timer:tc(fun() ->
        paginate_all(State, undefined, [])
    end),

    io:format("Paginated ~w resources in ~w μs (~.2f ms)~n",
              [N, Time, Time / 1000]),
    io:format("Per-page latency: ~.2f ms~n",
              [Time / 1000 / ((N + 99) div 100)]).

paginate_all(State, undefined, Acc) ->
    {ok, Resources, NextCursor} = erlmcp_server:list_resources(State, undefined),
    paginate_all(State, NextCursor, [Resources | Acc]);
paginate_all(State, Cursor, Acc) when is_binary(Cursor) ->
    {ok, Resources, NextCursor} = erlmcp_server:list_resources(State, Cursor),
    paginate_all(State, NextCursor, [Resources | Acc]);
paginate_all(_State, undefined, Acc) ->
    lists:reverse(Acc).
```

**Expected Results**:
- 1,000 resources: ~5ms total, ~0.5ms per page
- 10,000 resources: ~50ms total, ~0.5ms per page
- 100,000 resources: ~500ms total, ~0.5ms per page

### Benchmark 2: Read Latency

```erlang
benchmark_read(N) ->
    %% Setup: Create resource
    URI = <<"file:///test.txt">>,
    Content = <<"Test content">>,
    Handler = fun(_) -> Content end,
    Resource = #mcp_resource{uri = URI, name = <<"Test">>},

    State = #mcp_server_state{
        resources = #{URI => {Resource, Handler}}
    },

    %% Benchmark: Read resource N times
    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            {ok, _ContentObject} = erlmcp_server:read_resource(State, URI)
        end, lists:seq(1, N))
    end),

    io:format("Read resource ~w times in ~w μs~n", [N, Time]),
    io:format("Per-read latency: ~.2f μs (~.2f ms)~n",
              [Time / N, Time / N / 1000]).
```

**Expected Results**:
- Per-read latency: <10 μs (<0.01 ms)
- Throughput: >100,000 reads/sec per process

---

## Document Metadata

- **Version**: 1.0
- **Date**: 2026-01-31
- **Author**: Claude (Anthropic)
- **Status**: Draft
- **Target Completion**: Week of 2026-02-03

**Revision History**:
- 2026-01-31: Initial draft (v1.0)

**Related Documents**:
- MCP Specification 2025-11-25
- CLAUDE.md (Development guide)
- Phase 2b: Tools API Implementation (future)
- Phase 2c: Prompts API Implementation (future)

---

**END OF DOCUMENT**
