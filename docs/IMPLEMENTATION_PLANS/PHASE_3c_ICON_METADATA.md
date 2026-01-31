# Phase 3c: Icon Metadata Integration - Implementation Plan

**Status**: Planning
**Priority**: P2 (Nice-to-have)
**Effort**: 8-12 hours
**Impact**: Enhanced UI/UX in client applications
**Created**: 2026-01-31
**MCP Spec**: 2025-11-25 (Optional icon field)

---

## 1. Overview (Executive Summary)

### Current State
erlmcp has a sophisticated icon caching system (`erlmcp_icon_cache.erl`) with:
- LRU eviction policy for memory management
- TTL-based expiration (default 1 hour)
- SHA-256 checksum validation for integrity
- 10MB default memory limit with configurable thresholds
- ETS-based storage for fast concurrent access

However, **icons are not integrated** with tools, resources, or prompts. The MCP 2025-11-25 specification allows optional icon metadata on these entities to improve visual representation in client applications.

### Target State
Enable icon metadata on all three core MCP entities:
- **Tools**: Visual identification in tool lists (e.g., calculator icon for math tools)
- **Resources**: Visual hints for resource types (e.g., file type icons)
- **Prompts**: Visual categorization of prompt templates

### Scope
This implementation adds:
1. `icon` field to `#mcp_tool{}`, `#mcp_resource{}`, `#mcp_prompt{}` records
2. Icon validation module (`erlmcp_icon_validator.erl`)
3. Encoding/decoding support for icon metadata
4. API functions: `add_*_with_icon/N`, `update_*_icon/2`, `get_*_icon/2`
5. Integration with existing icon cache for URL validation
6. Comprehensive test coverage (≥80%)

### Effort Breakdown
- **Planning & Design**: 1 hour
- **Record Modifications**: 1 hour
- **Tool Icon Implementation**: 2 hours
- **Resource Icon Implementation**: 1.5 hours
- **Prompt Icon Implementation**: 1.5 hours
- **Icon Validation Module**: 1 hour
- **Testing**: 2 hours
- **Integration & Verification**: 1.5 hours
- **Documentation**: 0.5 hours
- **Total**: 8-12 hours

### Impact
- **User Experience**: Improved visual recognition in client UIs
- **Developer Experience**: Simple API to attach icons
- **Performance**: Negligible (icons cached, validated once)
- **Backward Compatibility**: 100% (icon field is optional)
- **Breaking Changes**: None

### Status
- **Phase**: Planning
- **Priority**: P2 (Low priority, nice-to-have feature)
- **Dependencies**: None (icon cache already exists)
- **Blocking**: Nothing
- **Risk**: Low (optional feature, non-breaking)

---

## 2. Current State Analysis

### Existing Icon Cache (`erlmcp_icon_cache.erl`)

**Capabilities**:
- Cache key: `{IconType, Uri, Size}` tuple
- Cache entry: `#icon_entry{data, last_access, size, checksum}`
- LRU eviction when memory limit exceeded (default 10MB)
- TTL-based expiration (default 1 hour, configurable)
- Periodic cleanup every 5 minutes
- Atomic ETS operations (public table, concurrent read/write)
- Statistics tracking: hit rate, miss rate, eviction count, memory usage

**API**:
```erlang
erlmcp_icon_cache:get(Type, Uri, Size) -> {ok, Data} | {error, not_found | expired}
erlmcp_icon_cache:put(Type, Uri, Size, Data, TTLMs) -> ok
erlmcp_icon_cache:invalidate(Type, Uri) -> ok
erlmcp_icon_cache:clear() -> ok
erlmcp_icon_cache:stats() -> #{hits, misses, cache_size, memory_bytes, hit_rate, ...}
erlmcp_icon_cache:memory_usage() -> {ok, Current, Max}
```

**Limitations**:
1. **No URL validation** - Accepts any binary as URI
2. **No size limits on download** - Could cache arbitrarily large icons
3. **No MIME type validation** - Could cache non-image data
4. **Not integrated with MCP records** - Standalone module

### Record Definitions (erlmcp.hrl)

**Current Tool Record** (line 827):
```erlang
-record(mcp_tool, {
    name :: binary(),
    description :: binary(),
    input_schema :: map() | undefined,
    metadata :: map() | undefined,
    experimental = undefined :: map() | undefined,
    version :: binary() | undefined,
    deprecated = false :: boolean()
}).
```

**Current Resource Record** (line 807):
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

**Current Prompt Record** (line 843):
```erlang
-record(mcp_prompt, {
    name :: binary(),
    description :: binary() | undefined,
    arguments :: [#mcp_prompt_argument{}] | undefined,
    input_schema :: map() | undefined
}).
```

### Encoding/Decoding Modules

**erlmcp_tool.erl** (lines 94-136):
- `encode_tool/1`: Encodes `#mcp_tool{}` to JSON map
- `decode_tool/1`: Decodes JSON map to `#mcp_tool{}`
- Currently handles: name, description, inputSchema, metadata, experimental, version

**erlmcp_resource.erl** (lines 43-79):
- `encode_resource/1`: Encodes `#mcp_resource{}` to JSON map
- `decode_resource/1`: Decodes JSON map to `#mcp_resource{}`
- Currently handles: uri, name, description, mimeType, metadata

**erlmcp_prompts.erl**: Module exists but not examined (assumed similar pattern)

### API Functions (erlmcp_server.erl)

**Current Tool API** (lines 13-16):
```erlang
add_tool(Server, Name, Handler)
add_tool_with_description(Server, Name, Description, Handler)
add_tool_with_schema(Server, Name, Handler, Schema)
add_tool_full(Server, Name, Description, Handler, Schema)
```

**Current Resource API** (lines 11-12):
```erlang
add_resource(Server, Uri, Handler)
add_resource_template(Server, UriTemplate, Name, Handler)
```

**Current Prompt API** (lines 17-19):
```erlang
add_prompt(Server, Name, Handler)
add_prompt_with_args(Server, Name, Description, Handler, Args)
add_prompt_with_args_and_schema(Server, Name, Description, Handler, Args, Schema)
```

---

## 3. Icon Data Type Definition

### Icon Type Specification

**Type Definition** (add to erlmcp.hrl):
```erlang
%% Icon metadata for tools, resources, and prompts
%% Per MCP 2025-11-25 specification (optional field)
-type icon() :: #{
    url := binary(),              % REQUIRED: HTTP(S) URL to icon
    alt_text => binary(),         % OPTIONAL: Alt text for accessibility
    width => pos_integer(),       % OPTIONAL: Width in pixels
    height => pos_integer(),      % OPTIONAL: Height in pixels
    mime_type => binary()         % OPTIONAL: MIME type (image/png, image/svg+xml, etc.)
}.

-export_type([icon/0]).
```

### Validation Constraints

| Field | Type | Required | Constraints |
|-------|------|----------|-------------|
| `url` | binary | Yes | Valid HTTP(S) URL, max 2048 bytes |
| `alt_text` | binary | No | Max 256 bytes |
| `width` | pos_integer | No | 1 ≤ width ≤ 10000 pixels |
| `height` | pos_integer | No | 1 ≤ height ≤ 10000 pixels |
| `mime_type` | binary | No | Valid MIME type (image/*) |

### Allowed MIME Types
```erlang
-define(ICON_ALLOWED_MIME_TYPES, [
    <<"image/png">>,
    <<"image/jpeg">>,
    <<"image/jpg">>,
    <<"image/gif">>,
    <<"image/svg+xml">>,
    <<"image/webp">>,
    <<"image/bmp">>,
    <<"image/x-icon">>,  % .ico files
    <<"image/vnd.microsoft.icon">>
]).
```

### Size Limits
```erlang
-define(ICON_MAX_URL_LENGTH, 2048).
-define(ICON_MAX_ALT_TEXT_LENGTH, 256).
-define(ICON_MAX_WIDTH, 10000).
-define(ICON_MAX_HEIGHT, 10000).
-define(ICON_MAX_DOWNLOAD_SIZE, 102400).  % 100 KB max icon download
```

---

## 4. Record Modifications

### File: `/home/user/erlmcp/apps/erlmcp_core/include/erlmcp.hrl`

**Changes Required**: Add `icon` field to three records

#### 4.1 Tool Record Enhancement (line 827)

**BEFORE**:
```erlang
-record(mcp_tool, {
    name :: binary(),
    description :: binary(),
    input_schema :: map() | undefined,
    metadata :: map() | undefined,
    experimental = undefined :: map() | undefined,
    version :: binary() | undefined,
    deprecated = false :: boolean()
}).
```

**AFTER**:
```erlang
-record(mcp_tool, {
    name :: binary(),
    description :: binary(),
    input_schema :: map() | undefined,
    metadata :: map() | undefined,
    experimental = undefined :: map() | undefined,
    version :: binary() | undefined,
    deprecated = false :: boolean(),
    icon = undefined :: icon() | undefined  % NEW: Optional icon metadata
}).
```

#### 4.2 Resource Record Enhancement (line 807)

**BEFORE**:
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

**AFTER**:
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
    size :: integer() | undefined,
    icon = undefined :: icon() | undefined  % NEW: Optional icon metadata
}).
```

#### 4.3 Prompt Record Enhancement (line 843)

**BEFORE**:
```erlang
-record(mcp_prompt, {
    name :: binary(),
    description :: binary() | undefined,
    arguments :: [#mcp_prompt_argument{}] | undefined,
    input_schema :: map() | undefined
}).
```

**AFTER**:
```erlang
-record(mcp_prompt, {
    name :: binary(),
    description :: binary() | undefined,
    arguments :: [#mcp_prompt_argument{}] | undefined,
    input_schema :: map() | undefined,
    icon = undefined :: icon() | undefined  % NEW: Optional icon metadata
}).
```

**Total Lines Modified**: ~30 LOC (3 records × ~10 lines each)

---

## 5. Icon Validation Module

### File: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_icon_validator.erl` (NEW)

**Purpose**: Centralized icon validation logic

**Module Structure** (150 LOC):

```erlang
-module(erlmcp_icon_validator).

-include("erlmcp.hrl").
-include_lib("kernel/include/logger.hrl").

%% API exports
-export([
    validate/1,
    validate_url/1,
    validate_dimensions/2,
    validate_mime_type/1,
    validate_alt_text/1,
    is_valid_icon_url/1
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Validate complete icon metadata
-spec validate(icon() | undefined) -> ok | {error, term()}.
validate(undefined) ->
    ok;
validate(Icon) when is_map(Icon) ->
    case maps:get(url, Icon, undefined) of
        undefined ->
            {error, icon_url_required};
        URL ->
            case validate_url(URL) of
                ok ->
                    validate_optional_fields(Icon);
                Error ->
                    Error
            end
    end;
validate(_) ->
    {error, invalid_icon_format}.

%% @doc Validate icon URL (HTTP/HTTPS only)
-spec validate_url(binary()) -> ok | {error, term()}.
validate_url(URL) when is_binary(URL) ->
    case byte_size(URL) of
        Size when Size > ?ICON_MAX_URL_LENGTH ->
            {error, {icon_url_too_long, Size, ?ICON_MAX_URL_LENGTH}};
        Size when Size =:= 0 ->
            {error, icon_url_empty};
        _ ->
            %% Validate URL format using erlmcp_uri_validator
            case erlmcp_uri_validator:validate_uri(URL) of
                ok ->
                    validate_url_scheme(URL);
                {error, Reason} ->
                    {error, {invalid_icon_url, Reason}}
            end
    end;
validate_url(_) ->
    {error, icon_url_must_be_binary}.

%% @doc Validate icon dimensions (both width and height)
-spec validate_dimensions(pos_integer() | undefined, pos_integer() | undefined) ->
    ok | {error, term()}.
validate_dimensions(undefined, undefined) ->
    ok;
validate_dimensions(Width, Height)
  when is_integer(Width), is_integer(Height),
       Width > 0, Height > 0,
       Width =< ?ICON_MAX_WIDTH, Height =< ?ICON_MAX_HEIGHT ->
    ok;
validate_dimensions(Width, Height)
  when is_integer(Width), is_integer(Height) ->
    case {Width > ?ICON_MAX_WIDTH, Height > ?ICON_MAX_HEIGHT} of
        {true, _} ->
            {error, {icon_width_too_large, Width, ?ICON_MAX_WIDTH}};
        {_, true} ->
            {error, {icon_height_too_large, Height, ?ICON_MAX_HEIGHT}}
    end;
validate_dimensions(Width, _Height) when not is_integer(Width) ->
    {error, icon_width_must_be_integer};
validate_dimensions(_Width, Height) when not is_integer(Height) ->
    {error, icon_height_must_be_integer}.

%% @doc Validate icon MIME type
-spec validate_mime_type(binary() | undefined) -> ok | {error, term()}.
validate_mime_type(undefined) ->
    ok;
validate_mime_type(MimeType) when is_binary(MimeType) ->
    case lists:member(MimeType, ?ICON_ALLOWED_MIME_TYPES) of
        true ->
            ok;
        false ->
            {error, {unsupported_icon_mime_type, MimeType}}
    end;
validate_mime_type(_) ->
    {error, icon_mime_type_must_be_binary}.

%% @doc Validate alt text
-spec validate_alt_text(binary() | undefined) -> ok | {error, term()}.
validate_alt_text(undefined) ->
    ok;
validate_alt_text(AltText) when is_binary(AltText) ->
    case byte_size(AltText) of
        Size when Size > ?ICON_MAX_ALT_TEXT_LENGTH ->
            {error, {icon_alt_text_too_long, Size, ?ICON_MAX_ALT_TEXT_LENGTH}};
        _ ->
            ok
    end;
validate_alt_text(_) ->
    {error, icon_alt_text_must_be_binary}.

%% @doc Quick check if URL is a valid icon URL
-spec is_valid_icon_url(binary()) -> boolean().
is_valid_icon_url(URL) ->
    case validate_url(URL) of
        ok -> true;
        _ -> false
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Validate optional icon fields
-spec validate_optional_fields(icon()) -> ok | {error, term()}.
validate_optional_fields(Icon) ->
    AltText = maps:get(alt_text, Icon, undefined),
    Width = maps:get(width, Icon, undefined),
    Height = maps:get(height, Icon, undefined),
    MimeType = maps:get(mime_type, Icon, undefined),

    case validate_alt_text(AltText) of
        ok ->
            case validate_dimensions(Width, Height) of
                ok ->
                    validate_mime_type(MimeType);
                Error -> Error
            end;
        Error -> Error
    end.

%% @private Validate URL scheme (HTTP/HTTPS only)
-spec validate_url_scheme(binary()) -> ok | {error, term()}.
validate_url_scheme(URL) ->
    case binary:split(URL, <<"://">>, [global]) of
        [Scheme, _Rest] ->
            case binary:match(Scheme, [<<"http">>, <<"https">>]) of
                {0, _} -> ok;
                _ -> {error, {invalid_icon_url_scheme, Scheme}}
            end;
        _ ->
            {error, invalid_icon_url_format}
    end.
```

**Test Coverage Requirements**:
- Valid icon with all fields
- Valid icon with only URL
- Invalid URL format
- URL too long
- Invalid MIME type
- Dimensions too large
- Invalid alt text

---

## 6. Tool Icon Implementation

### 6.1 File: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_tool.erl`

**Modifications Required**: Update encode/decode functions (30 LOC)

#### Current `encode_tool/1` (lines 94-122)

**BEFORE**:
```erlang
-spec encode_tool(#mcp_tool{}) -> map().
encode_tool(#mcp_tool{
    name = Name,
    description = Desc,
    input_schema = Schema,
    metadata = Metadata,
    experimental = Experimental,
    version = Version
}) ->
    Base = #{
        <<"name">> => Name,
        <<"description">> => Desc
    },
    Base1 = case Schema of
        undefined -> Base;
        _ -> Base#{<<"inputSchema">> => Schema}
    end,
    Base2 = case Metadata of
        undefined -> Base1;
        _ -> Base1#{<<"metadata">> => Metadata}
    end,
    Base3 = case Experimental of
        undefined -> Base2;
        _ -> Base2#{<<"experimental">> => Experimental}
    end,
    case Version of
        undefined -> Base3;
        _ -> Base3#{<<"version">> => Version}
    end.
```

**AFTER**:
```erlang
-spec encode_tool(#mcp_tool{}) -> map().
encode_tool(#mcp_tool{
    name = Name,
    description = Desc,
    input_schema = Schema,
    metadata = Metadata,
    experimental = Experimental,
    version = Version,
    icon = Icon  % NEW
}) ->
    Base = #{
        <<"name">> => Name,
        <<"description">> => Desc
    },
    Base1 = case Schema of
        undefined -> Base;
        _ -> Base#{<<"inputSchema">> => Schema}
    end,
    Base2 = case Metadata of
        undefined -> Base1;
        _ -> Base1#{<<"metadata">> => Metadata}
    end,
    Base3 = case Experimental of
        undefined -> Base2;
        _ -> Base2#{<<"experimental">> => Experimental}
    end,
    Base4 = case Version of
        undefined -> Base3;
        _ -> Base3#{<<"version">> => Version}
    end,
    %% NEW: Encode icon field
    case Icon of
        undefined -> Base4;
        _ -> Base4#{<<"icon">> => encode_icon(Icon)}
    end.

%% @doc Encode icon map to JSON-compatible format
-spec encode_icon(icon() | undefined) -> map() | undefined.
encode_icon(undefined) ->
    undefined;
encode_icon(Icon) when is_map(Icon) ->
    Base = #{<<"url">> => maps:get(url, Icon)},
    Base1 = case maps:get(alt_text, Icon, undefined) of
        undefined -> Base;
        AltText -> Base#{<<"altText">> => AltText}
    end,
    Base2 = case maps:get(width, Icon, undefined) of
        undefined -> Base1;
        Width -> Base1#{<<"width">> => Width}
    end,
    Base3 = case maps:get(height, Icon, undefined) of
        undefined -> Base2;
        Height -> Base2#{<<"height">> => Height}
    end,
    case maps:get(mime_type, Icon, undefined) of
        undefined -> Base3;
        MimeType -> Base3#{<<"mimeType">> => MimeType}
    end.
```

#### Current `decode_tool/1` (lines 124-136)

**BEFORE**:
```erlang
-spec decode_tool(map()) -> #mcp_tool{}.
decode_tool(#{
    <<"name">> := Name,
    <<"description">> := Desc
} = Map) ->
    #mcp_tool{
        name = Name,
        description = Desc,
        input_schema = maps:get(<<"inputSchema">>, Map, undefined),
        metadata = maps:get(<<"metadata">>, Map, undefined),
        experimental = maps:get(<<"experimental">>, Map, undefined),
        version = maps:get(<<"version">>, Map, undefined)
    }.
```

**AFTER**:
```erlang
-spec decode_tool(map()) -> #mcp_tool{}.
decode_tool(#{
    <<"name">> := Name,
    <<"description">> := Desc
} = Map) ->
    #mcp_tool{
        name = Name,
        description = Desc,
        input_schema = maps:get(<<"inputSchema">>, Map, undefined),
        metadata = maps:get(<<"metadata">>, Map, undefined),
        experimental = maps:get(<<"experimental">>, Map, undefined),
        version = maps:get(<<"version">>, Map, undefined),
        icon = decode_icon(maps:get(<<"icon">>, Map, undefined))  % NEW
    }.

%% @doc Decode icon from JSON format to icon() map
-spec decode_icon(map() | undefined) -> icon() | undefined.
decode_icon(undefined) ->
    undefined;
decode_icon(IconMap) when is_map(IconMap) ->
    Icon = #{url => maps:get(<<"url">>, IconMap)},
    Icon1 = case maps:get(<<"altText">>, IconMap, undefined) of
        undefined -> Icon;
        AltText -> Icon#{alt_text => AltText}
    end,
    Icon2 = case maps:get(<<"width">>, IconMap, undefined) of
        undefined -> Icon1;
        Width -> Icon1#{width => Width}
    end,
    Icon3 = case maps:get(<<"height">>, IconMap, undefined) of
        undefined -> Icon2;
        Height -> Icon2#{height => Height}
    end,
    case maps:get(<<"mimeType">>, IconMap, undefined) of
        undefined -> Icon3;
        MimeType -> Icon3#{mime_type => MimeType}
    end.
```

### 6.2 File: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl`

**New API Functions** (80 LOC):

```erlang
%% @doc Add tool with icon metadata
-spec add_tool_with_icon(server(), binary(), binary(), icon(), tool_handler()) -> ok.
add_tool_with_icon(Server, Name, Description, Icon, Handler)
  when is_binary(Name), is_binary(Description), is_function(Handler, 1) ->
    %% Validate icon before adding
    case erlmcp_icon_validator:validate(Icon) of
        ok ->
            Tool = #mcp_tool{
                name = Name,
                description = Description,
                icon = Icon
            },
            gen_server:call(Server, {add_tool, Tool, Handler});
        {error, Reason} ->
            {error, {invalid_icon, Reason}}
    end.

%% @doc Update icon for an existing tool
-spec update_tool_icon(server(), binary(), icon() | undefined) -> ok | {error, term()}.
update_tool_icon(Server, ToolName, Icon) when is_binary(ToolName) ->
    case erlmcp_icon_validator:validate(Icon) of
        ok ->
            gen_server:call(Server, {update_tool_icon, ToolName, Icon});
        {error, Reason} ->
            {error, {invalid_icon, Reason}}
    end.

%% @doc Get icon for a tool
-spec get_tool_icon(server(), binary()) -> {ok, icon()} | {error, term()}.
get_tool_icon(Server, ToolName) when is_binary(ToolName) ->
    gen_server:call(Server, {get_tool_icon, ToolName}).
```

**gen_server callbacks** (add to handle_call):

```erlang
%% Handle update_tool_icon
handle_call({update_tool_icon, ToolName, Icon}, _From, State) ->
    case maps:get(ToolName, State#state.tools, undefined) of
        undefined ->
            {reply, {error, tool_not_found}, State};
        {Tool, Handler, Config} ->
            UpdatedTool = Tool#mcp_tool{icon = Icon},
            NewTools = maps:put(ToolName, {UpdatedTool, Handler, Config}, State#state.tools),
            {reply, ok, State#state{tools = NewTools}}
    end;

%% Handle get_tool_icon
handle_call({get_tool_icon, ToolName}, _From, State) ->
    case maps:get(ToolName, State#state.tools, undefined) of
        undefined ->
            {reply, {error, tool_not_found}, State};
        {Tool, _Handler, _Config} ->
            Icon = Tool#mcp_tool.icon,
            {reply, {ok, Icon}, State}
    end;
```

---

## 7. Resource Icon Implementation

### 7.1 File: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_resource.erl`

**Modifications** (30 LOC):

**Current `encode_resource/1` Enhancement**:
```erlang
-spec encode_resource(#mcp_resource{}) -> map().
encode_resource(#mcp_resource{
    uri = Uri,
    name = Name,
    description = Desc,
    mime_type = MimeType,
    metadata = Metadata,
    icon = Icon  % NEW
}) ->
    Base = #{
        <<"uri">> => Uri,
        <<"name">> => Name
    },
    Base1 = case Desc of
        undefined -> Base;
        _ -> Base#{<<"description">> => Desc}
    end,
    Base2 = case MimeType of
        undefined -> Base1;
        _ -> Base1#{<<"mimeType">> => MimeType}
    end,
    Base3 = case Metadata of
        undefined -> Base2;
        _ -> Base2#{<<"metadata">> => Metadata}
    end,
    %% NEW: Encode icon
    case Icon of
        undefined -> Base3;
        _ -> Base3#{<<"icon">> => erlmcp_tool:encode_icon(Icon)}
    end.
```

**Current `decode_resource/1` Enhancement**:
```erlang
-spec decode_resource(map()) -> #mcp_resource{}.
decode_resource(#{
    <<"uri">> := Uri,
    <<"name">> := Name
} = Map) ->
    #mcp_resource{
        uri = Uri,
        name = Name,
        description = maps:get(<<"description">>, Map, undefined),
        mime_type = maps:get(<<"mimeType">>, Map, undefined),
        metadata = maps:get(<<"metadata">>, Map, undefined),
        icon = erlmcp_tool:decode_icon(maps:get(<<"icon">>, Map, undefined))  % NEW
    }.
```

### 7.2 File: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl`

**New API Functions**:
```erlang
%% @doc Add resource with icon metadata
-spec add_resource_with_icon(server(), binary(), binary(), icon(), resource_handler()) -> ok.
add_resource_with_icon(Server, Uri, Name, Icon, Handler)
  when is_binary(Uri), is_binary(Name), is_function(Handler, 1) ->
    case erlmcp_icon_validator:validate(Icon) of
        ok ->
            Resource = #mcp_resource{
                uri = Uri,
                name = Name,
                icon = Icon
            },
            gen_server:call(Server, {add_resource_with_icon, Resource, Handler});
        {error, Reason} ->
            {error, {invalid_icon, Reason}}
    end.

%% @doc Update resource icon
-spec update_resource_icon(server(), binary(), icon() | undefined) -> ok | {error, term()}.
update_resource_icon(Server, ResourceUri, Icon) when is_binary(ResourceUri) ->
    case erlmcp_icon_validator:validate(Icon) of
        ok ->
            gen_server:call(Server, {update_resource_icon, ResourceUri, Icon});
        {error, Reason} ->
            {error, {invalid_icon, Reason}}
    end.

%% @doc Get resource icon
-spec get_resource_icon(server(), binary()) -> {ok, icon()} | {error, term()}.
get_resource_icon(Server, ResourceUri) when is_binary(ResourceUri) ->
    gen_server:call(Server, {get_resource_icon, ResourceUri}).
```

---

## 8. Prompt Icon Implementation

### 8.1 File: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_prompts.erl` (assumed to exist)

**Similar pattern to tools and resources** (30 LOC):

```erlang
%% encode_prompt/1 enhancement
-spec encode_prompt(#mcp_prompt{}) -> map().
encode_prompt(#mcp_prompt{
    name = Name,
    description = Desc,
    arguments = Args,
    input_schema = Schema,
    icon = Icon  % NEW
}) ->
    Base = #{<<"name">> => Name},
    Base1 = case Desc of
        undefined -> Base;
        _ -> Base#{<<"description">> => Desc}
    end,
    Base2 = case Args of
        undefined -> Base1;
        _ -> Base1#{<<"arguments">> => [encode_prompt_arg(A) || A <- Args]}
    end,
    Base3 = case Schema of
        undefined -> Base2;
        _ -> Base2#{<<"inputSchema">> => Schema}
    end,
    %% NEW: Encode icon
    case Icon of
        undefined -> Base3;
        _ -> Base3#{<<"icon">> => erlmcp_tool:encode_icon(Icon)}
    end.

%% decode_prompt/1 enhancement
-spec decode_prompt(map()) -> #mcp_prompt{}.
decode_prompt(#{<<"name">> := Name} = Map) ->
    #mcp_prompt{
        name = Name,
        description = maps:get(<<"description">>, Map, undefined),
        arguments = decode_prompt_args(maps:get(<<"arguments">>, Map, undefined)),
        input_schema = maps:get(<<"inputSchema">>, Map, undefined),
        icon = erlmcp_tool:decode_icon(maps:get(<<"icon">>, Map, undefined))  % NEW
    }.
```

### 8.2 Server API (erlmcp_server.erl)

```erlang
%% @doc Add prompt with icon
-spec add_prompt_with_icon(server(), binary(), binary(), icon(), prompt_handler()) -> ok.
add_prompt_with_icon(Server, Name, Description, Icon, Handler)
  when is_binary(Name), is_binary(Description), is_function(Handler, 1) ->
    case erlmcp_icon_validator:validate(Icon) of
        ok ->
            Prompt = #mcp_prompt{
                name = Name,
                description = Description,
                icon = Icon
            },
            gen_server:call(Server, {add_prompt, Prompt, Handler});
        {error, Reason} ->
            {error, {invalid_icon, Reason}}
    end.

%% @doc Update prompt icon
-spec update_prompt_icon(server(), binary(), icon() | undefined) -> ok | {error, term()}.
update_prompt_icon(Server, PromptName, Icon) when is_binary(PromptName) ->
    case erlmcp_icon_validator:validate(Icon) of
        ok ->
            gen_server:call(Server, {update_prompt_icon, PromptName, Icon});
        {error, Reason} ->
            {error, {invalid_icon, Reason}}
    end.

%% @doc Get prompt icon
-spec get_prompt_icon(server(), binary()) -> {ok, icon()} | {error, term()}.
get_prompt_icon(Server, PromptName) when is_binary(PromptName) ->
    gen_server:call(Server, {get_prompt_icon, PromptName}).
```

---

## 9. Icon Cache Enhancements

### File: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_icon_cache.erl`

**New Functions** (50 LOC):

```erlang
%% @doc Download and cache icon from URL
-spec cache_icon_from_url(binary()) -> {ok, binary()} | {error, term()}.
cache_icon_from_url(URL) ->
    %% Check cache first
    case get(icon, URL, default) of
        {ok, Data} ->
            {ok, Data};
        {error, not_found} ->
            download_and_cache(URL);
        {error, expired} ->
            download_and_cache(URL)
    end.

%% @doc Validate icon URL is accessible
-spec validate_icon_url(binary()) -> ok | {error, term()}.
validate_icon_url(URL) ->
    %% Quick HEAD request to validate URL exists
    case http_head(URL) of
        {ok, 200, Headers} ->
            validate_icon_content_type(Headers);
        {ok, StatusCode, _} ->
            {error, {invalid_status_code, StatusCode}};
        {error, Reason} ->
            {error, {url_not_accessible, Reason}}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Download icon and cache it
download_and_cache(URL) ->
    case download_icon(URL) of
        {ok, Data, MimeType} ->
            %% Validate size
            case byte_size(Data) =< ?ICON_MAX_DOWNLOAD_SIZE of
                true ->
                    %% Cache for 1 hour
                    put(icon, URL, default, Data, 3600000),
                    {ok, Data};
                false ->
                    {error, icon_too_large}
            end;
        {error, Reason} ->
            {error, {download_failed, Reason}}
    end.

%% @private Download icon via HTTP
download_icon(URL) ->
    %% Use httpc or gun to download
    %% Implementation depends on transport choice
    case httpc:request(get, {binary_to_list(URL), []}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, Headers, Body}} ->
            MimeType = extract_content_type(Headers),
            {ok, Body, MimeType};
        {ok, {{_, StatusCode, _}, _Headers, _Body}} ->
            {error, {http_error, StatusCode}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Extract Content-Type from HTTP headers
extract_content_type(Headers) ->
    case lists:keyfind("content-type", 1, Headers) of
        {_, ContentType} -> list_to_binary(ContentType);
        false -> undefined
    end.

%% @private Send HTTP HEAD request
http_head(URL) ->
    case httpc:request(head, {binary_to_list(URL), []}, [], []) of
        {ok, {{_, StatusCode, _}, Headers, _}} ->
            {ok, StatusCode, Headers};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Validate Content-Type is an image
validate_icon_content_type(Headers) ->
    case lists:keyfind("content-type", 1, Headers) of
        {_, ContentType} ->
            case binary:match(list_to_binary(ContentType), <<"image/">>) of
                {0, _} -> ok;
                _ -> {error, not_an_image}
            end;
        false ->
            {error, no_content_type}
    end.
```

---

## 10. Testing Plan

### 10.1 Icon Validation Tests

**File**: `/home/user/erlmcp/apps/erlmcp_core/test/erlmcp_icon_validator_tests.erl` (100 LOC)

```erlang
-module(erlmcp_icon_validator_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Valid Icon Tests
%%====================================================================

valid_icon_minimal_test() ->
    Icon = #{url => <<"https://example.com/icon.png">>},
    ?assertEqual(ok, erlmcp_icon_validator:validate(Icon)).

valid_icon_complete_test() ->
    Icon = #{
        url => <<"https://example.com/tool-icon.svg">>,
        alt_text => <<"Calculator icon">>,
        width => 64,
        height => 64,
        mime_type => <<"image/svg+xml">>
    },
    ?assertEqual(ok, erlmcp_icon_validator:validate(Icon)).

valid_icon_undefined_test() ->
    ?assertEqual(ok, erlmcp_icon_validator:validate(undefined)).

%%====================================================================
%% Invalid URL Tests
%%====================================================================

invalid_url_missing_test() ->
    Icon = #{alt_text => <<"Icon">>},
    ?assertMatch({error, icon_url_required}, erlmcp_icon_validator:validate(Icon)).

invalid_url_empty_test() ->
    Icon = #{url => <<>>},
    ?assertMatch({error, icon_url_empty}, erlmcp_icon_validator:validate(Icon)).

invalid_url_too_long_test() ->
    LongURL = binary:copy(<<"x">>, 3000),
    Icon = #{url => LongURL},
    ?assertMatch({error, {icon_url_too_long, _, _}}, erlmcp_icon_validator:validate(Icon)).

invalid_url_scheme_test() ->
    Icon = #{url => <<"ftp://example.com/icon.png">>},
    ?assertMatch({error, {invalid_icon_url_scheme, _}}, erlmcp_icon_validator:validate(Icon)).

invalid_url_format_test() ->
    Icon = #{url => <<"not a url">>},
    ?assertMatch({error, _}, erlmcp_icon_validator:validate(Icon)).

%%====================================================================
%% Dimension Tests
%%====================================================================

invalid_width_too_large_test() ->
    Icon = #{
        url => <<"https://example.com/icon.png">>,
        width => 20000,
        height => 64
    },
    ?assertMatch({error, {icon_width_too_large, _, _}}, erlmcp_icon_validator:validate(Icon)).

invalid_height_too_large_test() ->
    Icon = #{
        url => <<"https://example.com/icon.png">>,
        width => 64,
        height => 20000
    },
    ?assertMatch({error, {icon_height_too_large, _, _}}, erlmcp_icon_validator:validate(Icon)).

invalid_width_not_integer_test() ->
    Icon = #{
        url => <<"https://example.com/icon.png">>,
        width => <<"64">>,
        height => 64
    },
    ?assertMatch({error, icon_width_must_be_integer}, erlmcp_icon_validator:validate(Icon)).

%%====================================================================
%% MIME Type Tests
%%====================================================================

valid_mime_type_png_test() ->
    Icon = #{
        url => <<"https://example.com/icon.png">>,
        mime_type => <<"image/png">>
    },
    ?assertEqual(ok, erlmcp_icon_validator:validate(Icon)).

valid_mime_type_svg_test() ->
    Icon = #{
        url => <<"https://example.com/icon.svg">>,
        mime_type => <<"image/svg+xml">>
    },
    ?assertEqual(ok, erlmcp_icon_validator:validate(Icon)).

invalid_mime_type_test() ->
    Icon = #{
        url => <<"https://example.com/icon.pdf">>,
        mime_type => <<"application/pdf">>
    },
    ?assertMatch({error, {unsupported_icon_mime_type, _}}, erlmcp_icon_validator:validate(Icon)).

%%====================================================================
%% Alt Text Tests
%%====================================================================

valid_alt_text_test() ->
    Icon = #{
        url => <<"https://example.com/icon.png">>,
        alt_text => <<"Tool icon">>
    },
    ?assertEqual(ok, erlmcp_icon_validator:validate(Icon)).

invalid_alt_text_too_long_test() ->
    LongText = binary:copy(<<"x">>, 300),
    Icon = #{
        url => <<"https://example.com/icon.png">>,
        alt_text => LongText
    },
    ?assertMatch({error, {icon_alt_text_too_long, _, _}}, erlmcp_icon_validator:validate(Icon)).
```

### 10.2 Tool Icon Integration Tests

**File**: `/home/user/erlmcp/apps/erlmcp_core/test/erlmcp_tool_icon_integration_tests.erl` (120 LOC)

```erlang
-module(erlmcp_tool_icon_integration_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup() ->
    {ok, Server} = erlmcp_server:start_link(test_server, #mcp_server_capabilities{}),
    Server.

cleanup(Server) ->
    erlmcp_server:stop(Server).

%%====================================================================
%% Tool Icon Tests
%%====================================================================

add_tool_with_icon_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Server) ->
         [
             {"Add tool with valid icon",
              fun() ->
                  Icon = #{url => <<"https://example.com/calc.png">>},
                  Handler = fun(_Args) -> <<"result">> end,
                  ?assertEqual(ok, erlmcp_server:add_tool_with_icon(
                      Server, <<"calculator">>, <<"Math tool">>, Icon, Handler))
              end},

             {"Tool icon appears in list_tools response",
              fun() ->
                  Icon = #{
                      url => <<"https://example.com/tool.svg">>,
                      alt_text => <<"Tool icon">>,
                      width => 64,
                      height => 64,
                      mime_type => <<"image/svg+xml">>
                  },
                  Handler = fun(_) -> <<"ok">> end,
                  erlmcp_server:add_tool_with_icon(
                      Server, <<"my_tool">>, <<"Description">>, Icon, Handler),

                  %% Encode tool and verify icon is present
                  {ok, Tools} = erlmcp_server:list_tools(Server),
                  [EncodedTool|_] = Tools,
                  ?assertEqual(<<"https://example.com/tool.svg">>,
                               maps:get(<<"url">>, maps:get(<<"icon">>, EncodedTool)))
              end},

             {"Update tool icon",
              fun() ->
                  Icon1 = #{url => <<"https://example.com/old.png">>},
                  Icon2 = #{url => <<"https://example.com/new.png">>},
                  Handler = fun(_) -> <<"ok">> end,

                  erlmcp_server:add_tool_with_icon(
                      Server, <<"tool">>, <<"Desc">>, Icon1, Handler),
                  ?assertEqual(ok, erlmcp_server:update_tool_icon(
                      Server, <<"tool">>, Icon2)),

                  {ok, Icon} = erlmcp_server:get_tool_icon(Server, <<"tool">>),
                  ?assertEqual(<<"https://example.com/new.png">>, maps:get(url, Icon))
              end},

             {"Get tool icon",
              fun() ->
                  Icon = #{
                      url => <<"https://example.com/icon.png">>,
                      width => 128,
                      height => 128
                  },
                  Handler = fun(_) -> <<"ok">> end,
                  erlmcp_server:add_tool_with_icon(
                      Server, <<"tool">>, <<"Desc">>, Icon, Handler),

                  {ok, ReturnedIcon} = erlmcp_server:get_tool_icon(Server, <<"tool">>),
                  ?assertEqual(Icon, ReturnedIcon)
              end},

             {"Add tool with invalid icon returns error",
              fun() ->
                  InvalidIcon = #{url => <<"not a url">>},
                  Handler = fun(_) -> <<"ok">> end,
                  ?assertMatch({error, {invalid_icon, _}},
                      erlmcp_server:add_tool_with_icon(
                          Server, <<"tool">>, <<"Desc">>, InvalidIcon, Handler))
              end}
         ]
     end}.

encode_decode_tool_icon_test() ->
    Icon = #{
        url => <<"https://example.com/icon.png">>,
        alt_text => <<"Icon">>,
        width => 64,
        height => 64,
        mime_type => <<"image/png">>
    },
    Tool = #mcp_tool{
        name = <<"test_tool">>,
        description = <<"Test">>,
        icon = Icon
    },

    %% Encode
    Encoded = erlmcp_tool:encode_tool(Tool),
    ?assertMatch(#{<<"icon">> := #{<<"url">> := <<"https://example.com/icon.png">>}}, Encoded),

    %% Decode
    Decoded = erlmcp_tool:decode_tool(Encoded),
    ?assertEqual(Icon, Decoded#mcp_tool.icon).
```

### 10.3 Resource Icon Tests

**File**: `/home/user/erlmcp/apps/erlmcp_core/test/erlmcp_resource_icon_integration_tests.erl` (120 LOC)

Similar structure to tool icon tests, testing:
- `add_resource_with_icon/5`
- `update_resource_icon/3`
- `get_resource_icon/2`
- Encoding/decoding with icon field

### 10.4 Prompt Icon Tests

**File**: `/home/user/erlmcp/apps/erlmcp_core/test/erlmcp_prompt_icon_integration_tests.erl` (120 LOC)

Similar structure to tool/resource tests.

---

## 11. API Summary

### New Public API Functions

#### Tool Icon API
```erlang
erlmcp_server:add_tool_with_icon(Server, Name, Description, Icon, Handler) -> ok | {error, term()}
erlmcp_server:update_tool_icon(Server, ToolName, Icon) -> ok | {error, term()}
erlmcp_server:get_tool_icon(Server, ToolName) -> {ok, icon()} | {error, term()}
```

#### Resource Icon API
```erlang
erlmcp_server:add_resource_with_icon(Server, Uri, Name, Icon, Handler) -> ok | {error, term()}
erlmcp_server:update_resource_icon(Server, ResourceUri, Icon) -> ok | {error, term()}
erlmcp_server:get_resource_icon(Server, ResourceUri) -> {ok, icon()} | {error, term()}
```

#### Prompt Icon API
```erlang
erlmcp_server:add_prompt_with_icon(Server, Name, Description, Icon, Handler) -> ok | {error, term()}
erlmcp_server:update_prompt_icon(Server, PromptName, Icon) -> ok | {error, term()}
erlmcp_server:get_prompt_icon(Server, PromptName) -> {ok, icon()} | {error, term()}
```

#### Icon Validation API
```erlang
erlmcp_icon_validator:validate(Icon) -> ok | {error, term()}
erlmcp_icon_validator:validate_url(URL) -> ok | {error, term()}
erlmcp_icon_validator:validate_dimensions(Width, Height) -> ok | {error, term()}
erlmcp_icon_validator:is_valid_icon_url(URL) -> boolean()
```

#### Icon Cache API (Enhanced)
```erlang
erlmcp_icon_cache:cache_icon_from_url(URL) -> {ok, binary()} | {error, term()}
erlmcp_icon_cache:validate_icon_url(URL) -> ok | {error, term()}
```

---

## 12. Files Modified/Created Summary

### Files to CREATE (5 files, ~610 LOC)

| File | LOC | Purpose |
|------|-----|---------|
| `erlmcp_icon_validator.erl` | 150 | Icon validation logic |
| `erlmcp_icon_validator_tests.erl` | 100 | Icon validator unit tests |
| `erlmcp_tool_icon_integration_tests.erl` | 120 | Tool icon integration tests |
| `erlmcp_resource_icon_integration_tests.erl` | 120 | Resource icon integration tests |
| `erlmcp_prompt_icon_integration_tests.erl` | 120 | Prompt icon integration tests |
| **Total** | **610** | |

### Files to MODIFY (7 files, ~190 LOC)

| File | Lines Modified | Changes |
|------|----------------|---------|
| `erlmcp.hrl` | 10 | Add icon type + 3 record field additions |
| `erlmcp_tool.erl` | 30 | encode/decode icon support |
| `erlmcp_resource.erl` | 30 | encode/decode icon support |
| `erlmcp_prompts.erl` | 30 | encode/decode icon support |
| `erlmcp_server.erl` | 80 | New API functions + handlers |
| `erlmcp_icon_cache.erl` | 50 | URL validation + download helpers |
| **Total** | **~230** | |

### Grand Total
- **New Code**: 610 LOC
- **Modified Code**: 230 LOC
- **Total Implementation**: ~840 LOC

---

## 13. Example Usage

### 13.1 Adding a Tool with Icon

```erlang
%% Start server
{ok, Server} = erlmcp_server:start_link(my_server, #mcp_server_capabilities{}),

%% Define icon
Icon = #{
    url => <<"https://cdn.example.com/icons/calculator.svg">>,
    alt_text => <<"Calculator icon">>,
    width => 64,
    height => 64,
    mime_type => <<"image/svg+xml">>
},

%% Add tool with icon
Handler = fun(#{<<"operation">> := Op, <<"a">> := A, <<"b">> := B}) ->
    case Op of
        <<"add">> -> integer_to_binary(A + B);
        <<"subtract">> -> integer_to_binary(A - B);
        _ -> <<"unknown operation">>
    end
end,

ok = erlmcp_server:add_tool_with_icon(
    Server,
    <<"calculator">>,
    <<"Simple calculator tool">>,
    Icon,
    Handler
).
```

### 13.2 Client Receives Tool with Icon

**JSON-RPC Request**:
```json
{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/list",
    "params": {}
}
```

**JSON-RPC Response**:
```json
{
    "jsonrpc": "2.0",
    "id": 1,
    "result": {
        "tools": [
            {
                "name": "calculator",
                "description": "Simple calculator tool",
                "inputSchema": {
                    "type": "object",
                    "properties": {
                        "operation": {"type": "string", "enum": ["add", "subtract"]},
                        "a": {"type": "number"},
                        "b": {"type": "number"}
                    },
                    "required": ["operation", "a", "b"]
                },
                "icon": {
                    "url": "https://cdn.example.com/icons/calculator.svg",
                    "altText": "Calculator icon",
                    "width": 64,
                    "height": 64,
                    "mimeType": "image/svg+xml"
                }
            }
        ]
    }
}
```

### 13.3 Adding a Resource with Icon

```erlang
Icon = #{
    url => <<"https://cdn.example.com/icons/document.png">>,
    alt_text => <<"Document icon">>,
    mime_type => <<"image/png">>
},

ResourceHandler = fun(Uri) ->
    %% Read file contents
    {ok, Content} = file:read_file(Uri),
    Content
end,

ok = erlmcp_server:add_resource_with_icon(
    Server,
    <<"file:///path/to/document.txt">>,
    <<"My Document">>,
    Icon,
    ResourceHandler
).
```

### 13.4 Updating Icon for Existing Tool

```erlang
%% Update to a new icon
NewIcon = #{
    url => <<"https://cdn.example.com/icons/calculator-v2.svg">>,
    width => 128,
    height => 128
},

ok = erlmcp_server:update_tool_icon(Server, <<"calculator">>, NewIcon).
```

### 13.5 Removing Icon (Set to undefined)

```erlang
ok = erlmcp_server:update_tool_icon(Server, <<"calculator">>, undefined).
```

---

## 14. Timeline & Milestones

### Phase 1: Foundation (2 hours)
- [x] **Hour 1**: Planning & design (this document)
- [ ] **Hour 2**: Record modifications in `erlmcp.hrl`
  - Add `icon()` type definition
  - Add `icon` field to 3 records
  - Compile and verify no errors

### Phase 2: Icon Validation (1.5 hours)
- [ ] **Hour 3**: Implement `erlmcp_icon_validator.erl`
  - URL validation (HTTP/HTTPS only)
  - Dimension validation (1-10000 pixels)
  - MIME type validation
  - Alt text validation
- [ ] **Hour 3.5**: Write unit tests (`erlmcp_icon_validator_tests.erl`)
  - 15+ test cases
  - Run: `rebar3 eunit --module=erlmcp_icon_validator_tests`

### Phase 3: Tool Icons (2 hours)
- [ ] **Hour 4**: Modify `erlmcp_tool.erl`
  - Update `encode_tool/1`
  - Update `decode_tool/1`
  - Add `encode_icon/1` and `decode_icon/1` helpers
- [ ] **Hour 5**: Modify `erlmcp_server.erl`
  - Add `add_tool_with_icon/5`
  - Add `update_tool_icon/3`
  - Add `get_tool_icon/2`
  - Add gen_server handlers
- [ ] **Hour 5.5**: Integration tests
  - Write `erlmcp_tool_icon_integration_tests.erl`
  - Run: `rebar3 eunit --module=erlmcp_tool_icon_integration_tests`

### Phase 4: Resource Icons (1.5 hours)
- [ ] **Hour 6**: Modify `erlmcp_resource.erl` (encode/decode)
- [ ] **Hour 6.5**: Modify `erlmcp_server.erl` (API functions)
- [ ] **Hour 7**: Integration tests (`erlmcp_resource_icon_integration_tests.erl`)

### Phase 5: Prompt Icons (1.5 hours)
- [ ] **Hour 8**: Modify `erlmcp_prompts.erl` (encode/decode)
- [ ] **Hour 8.5**: Modify `erlmcp_server.erl` (API functions)
- [ ] **Hour 9**: Integration tests (`erlmcp_prompt_icon_integration_tests.erl`)

### Phase 6: Icon Cache Enhancements (1 hour)
- [ ] **Hour 10**: Enhance `erlmcp_icon_cache.erl`
  - Add `cache_icon_from_url/1`
  - Add `validate_icon_url/1`
  - Add download helpers
  - Test URL validation

### Phase 7: Integration & Verification (1.5 hours)
- [ ] **Hour 11**: Full system integration
  - Run all tests: `rebar3 eunit`
  - Run Common Test suites: `rebar3 ct`
  - Verify ≥80% coverage
- [ ] **Hour 11.5**: Quality gates
  - `TERM=dumb rebar3 compile` (0 errors)
  - `rebar3 dialyzer` (0 warnings)
  - `rebar3 xref` (0 issues)
- [ ] **Hour 12**: Manual testing
  - Start server with tools/resources/prompts with icons
  - Verify JSON-RPC responses contain icon metadata
  - Test icon updates and removals

### Phase 8: Documentation (0.5 hours)
- [ ] **Hour 12.5**: Update documentation
  - Add icon examples to `docs/api-reference.md`
  - Update `CHANGELOG.md` with icon feature
  - Update `README.md` with icon capabilities

---

## 15. Deployment & Rollout

### 15.1 Backward Compatibility

**100% Backward Compatible**:
- Icon field is **optional** (`undefined` by default)
- Existing code without icons continues to work
- Clients without icon support ignore the field
- No breaking changes to any API

**Migration Path**:
1. **Phase 1**: Deploy icon support (no icons added yet)
2. **Phase 2**: Gradually add icons to high-value tools/resources
3. **Phase 3**: Client applications implement icon rendering

### 15.2 Feature Flag (Optional)

```erlang
%% In sys.config
{erlmcp, [
    {enable_icons, true},  %% Default: true
    {icon_cache_max_memory, 10485760},  %% 10 MB
    {icon_download_timeout_ms, 5000}
]}.
```

### 15.3 Deployment Checklist

- [ ] All tests pass (100% pass rate)
- [ ] Coverage ≥80%
- [ ] Dialyzer clean (0 warnings)
- [ ] Xref clean (0 issues)
- [ ] Documentation updated
- [ ] CHANGELOG.md updated
- [ ] Release notes prepared
- [ ] Backward compatibility verified
- [ ] Performance impact measured (negligible expected)

---

## 16. Performance Considerations

### 16.1 Memory Impact

**Icon Cache Memory**:
- Default: 10 MB limit
- LRU eviction prevents unbounded growth
- Typical icon size: 1-5 KB (PNG/SVG)
- **Capacity**: ~2000-10000 cached icons

**Record Size Impact**:
- `#mcp_tool{}` size increase: ~8 bytes (pointer to icon map or undefined)
- `#mcp_resource{}` size increase: ~8 bytes
- `#mcp_prompt{}` size increase: ~8 bytes
- **Impact**: Negligible (<1% memory increase)

### 16.2 Network Impact

**Icon Downloads**:
- Only on first access (then cached)
- Async download (doesn't block requests)
- Max download size: 100 KB enforced
- **Impact**: One-time cost per unique icon URL

**JSON-RPC Message Size**:
- Icon adds ~100-300 bytes per tool/resource/prompt
- Only sent in `list` responses
- **Impact**: <5% message size increase

### 16.3 Latency Impact

**Validation Overhead**:
- Icon validation: <1ms (simple map checks)
- URL validation: <1ms (regex + scheme check)
- **Impact**: Negligible (<0.1% latency increase)

### 16.4 Benchmark Goals

- Icon validation: <1ms per icon
- Icon cache hit: <10μs (ETS lookup)
- Icon cache miss + download: <1s (network-bound)
- Encode/decode overhead: <5% vs non-icon version

---

## 17. Security Considerations

### 17.1 URL Validation

**Threats**:
- SSRF (Server-Side Request Forgery) via malicious URLs
- Privacy leaks via tracking pixels
- Data exfiltration via custom protocols

**Mitigations**:
1. **Scheme whitelist**: Only HTTP/HTTPS allowed (no `file://`, `ftp://`, etc.)
2. **URL length limit**: Max 2048 bytes
3. **Download size limit**: Max 100 KB
4. **MIME type validation**: Only `image/*` types allowed
5. **Timeout enforcement**: 5s max download time

### 17.2 Content Validation

**Threats**:
- Malicious SVG with embedded JavaScript
- XXE (XML External Entity) attacks in SVG
- Malformed images causing crashes

**Mitigations**:
1. **MIME type enforcement**: Validate Content-Type header
2. **Size limits**: Reject downloads >100 KB
3. **Optional**: SVG sanitization (future enhancement)

### 17.3 Privacy

**Considerations**:
- Icon URLs may leak information to CDN providers
- Client IP addresses visible to icon hosts

**Best Practices**:
1. Use trusted CDNs for icon hosting
2. Document privacy implications
3. Allow icon URLs to be proxied (future enhancement)

---

## 18. Future Enhancements (Out of Scope)

### 18.1 Icon Format Conversion
- Convert icons to standard sizes (16x16, 32x32, 64x64)
- Generate WebP versions for better compression
- **Effort**: 8 hours

### 18.2 Icon Proxy Service
- Proxy external icon URLs through erlmcp server
- Privacy protection (hide client IPs)
- **Effort**: 12 hours

### 18.3 SVG Sanitization
- Remove JavaScript from SVG icons
- Prevent XXE attacks
- **Effort**: 6 hours

### 18.4 Data URI Support
- Support `data:image/png;base64,...` URIs
- Eliminate external dependencies
- **Effort**: 4 hours

### 18.5 Icon Theme Support
- Light/dark theme variants
- System theme detection
- **Effort**: 8 hours

---

## 19. Quality Gates (Mandatory Before Merge)

### Pre-Merge Checklist

```bash
# 1. Compilation (MUST pass)
✅ TERM=dumb rebar3 compile
   Expected: 0 errors, 0 warnings

# 2. Unit Tests (MUST pass)
✅ rebar3 eunit
   Expected: 100% pass rate, 0 failures

# 3. Integration Tests (MUST pass)
✅ rebar3 ct
   Expected: All suites pass

# 4. Type Checking (SHOULD pass)
✅ rebar3 dialyzer
   Expected: 0 type warnings

# 5. Cross-Reference (SHOULD pass)
✅ rebar3 xref
   Expected: 0 undefined functions

# 6. Code Formatting (MUST pass)
✅ rebar3 format --verify
   Expected: No formatting issues

# 7. Coverage (MUST meet threshold)
✅ rebar3 cover
   Expected: ≥80% line coverage
```

### Quality Metrics

| Metric | Target | Actual | Status |
|--------|--------|--------|--------|
| Compilation Errors | 0 | TBD | ⏳ |
| Test Pass Rate | 100% | TBD | ⏳ |
| Code Coverage | ≥80% | TBD | ⏳ |
| Dialyzer Warnings | 0 | TBD | ⏳ |
| Xref Issues | 0 | TBD | ⏳ |
| Performance Regression | <5% | TBD | ⏳ |

---

## 20. Success Criteria

### Must Have (P0)
- [ ] Icon field added to all 3 record types
- [ ] Icon validation module implemented and tested
- [ ] Encoding/decoding support for all 3 entity types
- [ ] API functions for add/update/get icon
- [ ] All tests pass (≥80% coverage)
- [ ] 100% backward compatibility
- [ ] Zero performance regression (<5%)

### Should Have (P1)
- [ ] Icon cache enhancements (URL validation, download)
- [ ] Comprehensive error messages
- [ ] Example code in documentation

### Nice to Have (P2)
- [ ] Icon format conversion
- [ ] SVG sanitization
- [ ] Data URI support

---

## 21. Risk Assessment

### Low Risk
- **Backward Compatibility**: Icon is optional, no breaking changes
- **Performance**: Negligible memory/latency impact
- **Security**: URL validation prevents SSRF

### Medium Risk
- **Icon Cache Complexity**: Download failures, network timeouts
- **Mitigation**: Proper error handling, timeouts, fallback to undefined

### High Risk
- **None identified**

---

## 22. Conclusion

### Summary
Phase 3c: Icon Metadata Integration is a **low-risk, high-value** enhancement that adds visual metadata to tools, resources, and prompts. The implementation is straightforward (~840 LOC), fully backward-compatible, and leverages the existing icon cache infrastructure.

### Recommendation
**Proceed with implementation** as a P2 (nice-to-have) feature after higher-priority MCP compliance work is complete.

### Next Steps
1. Review this implementation plan
2. Get approval from project stakeholders
3. Create feature branch: `feature/icon-metadata-integration`
4. Follow timeline in Section 14
5. Submit PR with comprehensive tests and documentation

---

**Document Version**: 1.0
**Last Updated**: 2026-01-31
**Author**: Claude Code (Erlang OTP Developer Agent)
**Reviewed By**: TBD
**Status**: Planning Complete ✅
